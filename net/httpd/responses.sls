#!r6rs
;;; responses.sls --- HTTPd responses

;; Copyright (C) 2009-2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (ocelotl net httpd responses)
  (export make-error-response
          
          make-input-response
          input-response?
          input-response-handler
          
          xhtml-doctype
          put-sxml-response-body
          put-sxml-response-body/tasks)
  (import (rnrs)
          (only (srfi :1) append-map)
          (spells match)
          (spells finite-types)
          (spells tracing)
          (wak ssax tree-trans)
          (ocelotl net uri)
          (ocelotl net http)
          (ocelotl net httpd)
          (ocelotl ssax-utils))

(define-record-type input-response
  (fields handler))

(define (make-error-response status request . args)
  (define (create-response headers title shtml extras)
    (make-http-response
     (http-request/version request)
     status
     #f
     (cons '(content-type . "text/html; charset=utf-8") headers)
     (lambda (iport oport httpd)
       (put-sxml-response-body
        oport
        xhtml-doctype
        `(html
          (head
           (title ,(or title (http-status/reason status))))
          (body
           ,shtml
           ,@(if (null? extras)
                 '()
                 `((p "Further Information: " ,(car extras) (br)
                      ,@(append-map (lambda (item)
                                      `(,item (br)))
                                    (cdr extras)))))))))))
  (finite-type-case http-status status
    ((moved-temp moved-perm)
     (let-args args (location)
       (let ((location-str (if (uri? location)
                               (uri->string location)
                               location)))
         (create-response
          `((location . ,location-str))
          "Document moved"
          `(p "This document has " ,(if (eq? status (http-status moved-temp))
                                        "temporarily"
                                        "permanently")
              " moved to a " (a (^ (href ,location-str)) "new location."))
          '()))))
    ((bad-request)
     (let-args args ()
       (create-response
        '()
        #f
        `(p "Client sent a query that this server could not understand.")
        '())))
    ((method-not-allowed)
     (create-response
      '()
      #f
      `(p "Method not allowed.")
      args))
    ((unauthorized)
     (let-args args (challenge . args-rest)
       (create-response
        `((www-authenticate . ,challenge))
        "Authorization required"
        `(p "Browser not authentication-capable or "
            "authentication failed.")
        args)))
    ((forbidden)
     (create-response
      '()
      "Request not allowed"
      `(p "Your client does not have permission to perform a "
          ,(http-request/method request) " operation "
          "on URL " ,(uri->string (http-request/uri request)) ".")
      args))
    ((not-found)
     (create-response
      '()
      "URL not found"
      `(p "The requested URL was not found on this server.")
      args))
    ((internal-error)
     (create-response
      '()
      #f
      `(p "The server encountered an internal error or"
          " misconfiguration and was unable to complete your request."
          ;; TODO: server admin address
          )
      args))
    ((not-implemented)
     (create-response
      '()
      #f
      `(p "This server does not currently implement "
          "the requested method (" (http-request/method request) ").")
      args))
    ((bad-gateway)
     (create-response
      '()
      #f
      `(p "An error occurred while waiting for the "
          "response of a gateway.")
      args))

    (else
     ;; Generic fallback
     (create-response
      '()
      #f
      `(p "The server was unable to handle your request.")
      args))))


;;; SXML body helpers

(define xhtml-doctype
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")

(define (put-sxml-response-body port doctype sxml)
  (call-with-port (transcoded-port port (make-transcoder (utf-8-codec)))
    (lambda (port)
      (put-string port doctype)
      (sxml->xml sxml port))))

(define (put-sxml-response-body/tasks httpd port doctype sxml)
  (call-with-port (transcoded-port port (make-transcoder (utf-8-codec)))
    (lambda (port)
      (put-string port doctype)
      (let* ((task-result #f)
             (sxml
              (pre-post-order
               sxml
               `((task *PREORDER* .
                       ,(lambda (tag proc)
                          (lambda (port)
                            (set! task-result
                                  (proc port (lambda (v)
                                               (flush-output-port port)
                                               (httpd/yield httpd v)))))))
                 (task-result *PREORDER* .
                              ,(lambda (tag)
                                 (lambda (port)
                                   (cond ((procedure? task-result)
                                          (task-result port))
                                         (task-result
                                          (sxml->xml task-result port))))))
                 (*DEFAULT* . ,list)))))
        (sxml->xml sxml port)))))

(define-syntax let-args
  (syntax-rules ()
    ((_ args-expr (id ... . rest-id) body ...)
     (match args-expr
       ((id ... . rest-id) body ...)))))

)

;; Local Variables:
;; scheme-indent-styles: ((let-args 2))
;; End:
