;;; soup-httpd.sls --- A http daemon based on libsoup

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (ocelotl net soup-httpd)
  (export httpd
          httpd/options
          httpd/add-timeout
          make-httpd-options
          with-port
          with-interface
          with-request-handler
          with-server-header
          with-debug?)
  (import (except (rnrs) define-record-type)
          (srfi :8 receive)
          (only (srfi :13) string-join)
          (spells string-utils)
          (spells record-types)
          (spells logging)
          (spells operations)
          (spells tracing)
          (spells foof-loop)
          (sbank soup)
          (sbank glib)
          (sbank glib-daemon)
          (ocelotl private utils)
          (ocelotl scheduler)
          (ocelotl net uri)
          (ocelotl net http)
          (ocelotl net httpd)
          (ocelotl net httpd make-options)
          (ocelotl net httpd options)
          (ocelotl net httpd responses))

(define-operation (httpd/start-task httpd proc flush))

(define (soup-server->httpd server options)
  (let ((n-active-tasks 0)
        (scheduler (make-scheduler))
        (task-counter 0))
    
    (define (new-task-id)
      (set! task-counter (+ task-counter 1))
      task-counter)
  
    (define (scheduler-idle-callback)
      (scheduler-work scheduler))

    (object #f
      ((httpd/options self) options)
      ((httpd/add-timeout self seconds proc)
       (g-timeout-add-seconds seconds proc))
      ((httpd/start-task self msg proc)
       ;; This code has quite some hair; the basic idea is that we
       ;; don't want to do chunked encoding when `proc' doesn't yield.
       (let* ((real-yield #f)
              (exit #f)
              (continue 
               (call/cc
                 (lambda (escape)
                   (let ((result
                          (proc (lambda (v)
                                  (if real-yield
                                      (real-yield v)
                                      (call/cc escape))))))
                     (and exit (exit result)))))))
         (and
           continue
           (let ((task-id (new-task-id))
                 (had-work? (scheduler-has-work? scheduler))
                 (http-version (send msg (get 'http-version)))
                 (message-finished? #f))
           
             (define (task-result-consumer val)
               (unless message-finished?
                 (send server (unpause-message msg)))
               (set! n-active-tasks (- n-active-tasks 1))
               (log/debug "task {0} finished; {1} still active"
                          task-id n-active-tasks))
           
             (define (task-yield-handler val)
               (cond (message-finished?
                      (log/debug "<task {0}> cancelled" task-id)
                      (set! n-active-tasks (- n-active-tasks 1))
                      #f)
                     ((> http-version 0)
                      (send server (unpause-message msg))
                      #t)
                     (else
                      #t)))
         
             (send msg (connect 'finished (lambda (msg)
                                            (set! message-finished? #t))))
             (scheduler-enqueue! scheduler
                                 (lambda (yield)
                                   (set! real-yield yield)
                                   (call/cc
                                     (lambda (k)
                                       (set! exit k)
                                       (continue))))
                                 task-result-consumer
                                 task-yield-handler)
             (cond ((> http-version 0)
                    (send (send msg (get-response-body))
                      (set-accumulate #f))
                    (send (send msg (get-response-headers))
                      (set-encoding 'chunked)))
                   (else
                    (send server (pause-message msg))))
             (set! n-active-tasks (+ n-active-tasks 1))
             (log/debug "task {0} enqueued; {1} now active"
                        task-id n-active-tasks)
             (when (not had-work?)
               (g-idle-add scheduler-idle-callback))
             task-id)))))))

(define httpd
  (case-lambda
    ((options)
     (httpd options (lambda (httpd) #f)))
    ((options init)
     (let ((address (send <soup-address>
                      (new (httpd-options-interface options)
                           (httpd-options-port options))))
           (main-loop (g-main-loop-new #f #f)))
       
       
       (send address (resolve-sync #f))
       (let ((server
               (send <soup-server>
                 (new* 'interface address
                       'server-header (httpd-options-server-header options)))))
         
         (let ((httpd (soup-server->httpd server options)))
           (cond ((httpd-options-request-handler options)
                  => (lambda (handler)
                       (send server
                         (add-handler #f (handler->soup-handler
                                          handler
                                          httpd)))))
                 (else
                  (error 'httpd "no request handler specified")))
           (init httpd))
       
         (g-install-signal-handler
          '(int)
          (lambda (sig)
            (log/info "Received signal {0}, exiting" sig)
            (send server (shutdown))
            (g-main-loop-quit main-loop)
            #f))
       
         (log/info "Waiting for requests...")
         (send server (run-async))
         (g-main-loop-run main-loop))))))

(define (with-exception-guard httpd request thunk)
  (guard (c ((http-error? c)
             (apply make-error-response
                    (http-error/status c)
                    (http-error/request c)
                    (append
                     (if (message-condition? c)
                         (list (condition-message c))
                         '())
                     (if (irritants-condition? c)
                         (condition-irritants c)
                         '()))))
            ((not (httpd-options-debug? (httpd/options httpd)))
             (make-error-response
              (http-status internal-error)
              request)))
    (thunk)))

(define (handler->soup-handler handler httpd)
  (lambda (server msg path query client)
    (let* ((iport (make-soup-input-port (send msg (get-request-body))))
           (request (make-http-request
                     (make-http-version 1 (send msg (get 'http-version)))
                     (send msg (get 'method))
                     (soup-uri->uri (send msg (get 'uri)))
                     (soup-headers->header-fields
                      (send msg (get-request-headers)))
                     iport)))
      (loop continue
            ((with response
                   (with-exception-guard httpd request
                     (lambda ()
                       (handler (trim-path (uri-path (http-request/uri request)))
                                request))))
             (until (http-response? response)))
        => (soup-message-set-response! httpd msg response request iport)
        (cond ((input-response? response)
               (continue (with-exception-guard httpd request
                           (lambda () ((input-response-handler response) iport)))))
              (else
               (error 'handler->soup-handler
                      "Invalid response from handler"
                      response)))))))

(define (soup-message-set-response! httpd msg response request iport)
  (let ((resp-hdrs (send msg (get-response-headers)))
        (message-finished? #f))
    (for-each (lambda (header-field)
                (send resp-hdrs
                  (append (header-field/name->string
                           (http-header-field/name header-field))
                          (http-header-field/value header-field))))
              (http-response/header-fields response))
    
    (receive (oport flush) (make-soup-output-port+flusher
                            (send msg (get-response-body)))
      (send msg
        (set-status-full (http-response/status-code response)
                         (http-response/reason response)))
      (send msg (connect 'finished
                         (lambda (msg)
                           (set! message-finished? #t))))
      (httpd/start-task
       httpd
       msg
       (lambda (yield)
         (call-with-port oport
           (lambda (oport)
             (put-http-response-body
              iport
              oport
              (http-response/body response)
              (object httpd
                ((httpd/yield httpd v)
                 (unless message-finished?
                   (flush))
                 (yield v))))))
         (unless message-finished?
           (flush)
           (send (send msg (get-response-body))
             (complete))
           (log-request request response)))))))

(define (trim-path path)
  (if (and (pair? path)
           (pair? (cdr path))
           (string=? (car path) ""))
      (cdr path)
      path))

(define (put-http-response-body iport oport body httpd)
  (body iport oport httpd))

(define (log-request request response)
  (log/request "{0} {1} {2}"
               (http-request/method request)
               (path->string (uri-path (http-request/uri request)))
               (http-response/status-code response)))

(define (path->string path)
  (string-join path "/"))

(define (header-field/name->string name)
  (cond ((string? name) name)
        (else
         (string-join (map string-titlecase
                           (string-split (symbol->string name) #\-))
                      "-"))))

(define (soup-uri->uri soup-uri)
  (let ((scheme (send soup-uri (get-scheme)))
        (user (send soup-uri (get-user)))
        (host (send soup-uri (get-host)))
        (port (send soup-uri (get-port)))
        (path (send soup-uri (get-path)))
        (query (send soup-uri (get-query)))
        (fragment (send soup-uri (get-fragment))))
    (make-uri scheme
              (make-uri-authority (or user "")
                                  host
                                  port)
              (string-split path #\/)
              query
              fragment)))

(define (soup-headers->header-fields soup-hdrs)
  (let ((header-fields '()))
    (send soup-hdrs
      (foreach (lambda (name value)
                 (set! header-fields
                       (cons 
                        (make-http-header-field
                         (string->symbol (string-downcase name))
                         value)
                        header-fields)))))
    (reverse header-fields)))

(define logger:ocelotl (make-logger root-logger 'ocelotl))
(define logger:ocelotl.soup-httpd (make-logger logger:ocelotl 'soup-httpd))
(define logger:ocelotl.soup-httpd.request
  (make-logger logger:ocelotl.soup-httpd 'request))

(define log/debug (make-ssubst-log logger:ocelotl.soup-httpd 'debug))
(define log/info (make-ssubst-log logger:ocelotl.soup-httpd 'info))
(define log/request (make-ssubst-log logger:ocelotl.soup-httpd.request 'info))

(define-record-type* task-result
  (make-task-result task-id msg val)
  ())

(define-functional-fields task-result
  task-id msg val)

)

;; Local Variables:
;; scheme-indent-styles: (sbank foof-loop (object 1))
;; End:
