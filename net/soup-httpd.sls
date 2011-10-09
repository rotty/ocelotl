;;; soup-httpd.sls --- A http daemon based on libsoup

;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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
          (wak fmt)
          (spells string-utils)
          (spells record-types)
          (spells logging)
          (spells operations)
          (spells tracing)
          (wak foof-loop)
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
               (task-log task-id "finished" n-active-tasks))
             
             (define (task-yield-handler val)
               (cond (message-finished?
                      (set! n-active-tasks (- n-active-tasks 1))
                      (task-log task-id "cancelled" n-active-tasks)
                      #f)
                     ((eq? http-version 'http-1-1)
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
             (cond ((eq? http-version 'http-1-1)
                    (send (send msg (get-response-body))
                      (set-accumulate #f))
                    (send (send msg (get-response-headers))
                      (set-encoding 'chunked)))
                   (else
                    (send server (pause-message msg))))
             (set! n-active-tasks (+ n-active-tasks 1))
             (task-log task-id "enqueued" n-active-tasks)
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
           (main-loop (send <g-main-loop> (new #f #f))))
       
       
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
            (httpd-log 'info "Received signal " sig ", exiting")
            (send server (disconnect))
            (send main-loop (quit))
            #f))
       
         (httpd-log 'info "Waiting for requests...")
         (send server (run-async))
         (send main-loop (run)))))))

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

(define (symbol->http-version symbol)
  (case symbol
    ((http-1-0) (make-http-version 1 0))
    ((http-1-1) (make-http-version 1 1))
    (else
     (assertion-violation 'symbol->http-version
                          "unexpected HTTP version" symbol))))

(define (handler->soup-handler handler httpd)
  (lambda (server msg path query client)
    (let* ((iport (make-soup-input-port (send msg (get-request-body))))
           (request (make-http-request
                     (symbol->http-version (send msg (get 'http-version)))
                     (send msg (get 'method))
                     (soup-uri->uri (send msg (get 'uri)))
                     (soup-headers->header-fields
                      (send msg (get-request-headers)))
                     iport)))
      (log-request 'debug request #f)
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
           (log-request 'info request response)))))))

(define (trim-path path)
  (if (and (pair? path)
           (pair? (cdr path))
           (string=? (car path) ""))
      (cdr path)
      path))

(define (put-http-response-body iport oport body httpd)
  (body iport oport httpd))

(define (log-request level request response)
  (request-log level
               (http-request/method request)
               " " (path->string (uri-path (http-request/uri request)))
               (if response
                   (cat " " (http-response/status-code response))
                   fmt-null)))

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

(define httpd-log (make-fmt-log logger:ocelotl.soup-httpd))
(define request-log (make-fmt-log logger:ocelotl.soup-httpd.request))

(define (task-log task-id action n-active-tasks)
  (httpd-log 'debug
             "task " task-id " " action ";" " " n-active-tasks " now active"))

(define-record-type* task-result
  (make-task-result task-id msg val)
  ())

(define-functional-fields task-result
  task-id msg val)

)

;; Local Variables:
;; scheme-indent-styles: (sbank foof-loop (object 1))
;; End:
