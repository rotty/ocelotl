;;; http.sls --- HTTP data types

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (ocelotl net http)
  (export make-http-header-field
          http-header-field/name
          http-header-field/value

          make-http-version
          http-version?
          http-version=?
          http-version<=?
          http-version/major
          http-version/minor
          
          make-http-response
          http-response?
          http-response/header-fields
          http-response/reason
          http-response/status-code
          http-response/status-type
          http-response/version
          http-response/body

          make-http-request
          http-request?
          http-request/method
          http-request/version
          http-request/uri
          http-request/uri-query-alist
          http-request/header-fields
          http-request/body

          http-status
          http-status?
          http-status/code
          http-status/reason
          
          http-error
          http-error?
          http-error/status
          http-error/request
          http-error/status-code
          http-error/reason)
  (import (rnrs base)
          (rnrs control)
          (rnrs unicode)
          (rnrs io ports)
          (rnrs bytevectors)
          (rnrs conditions)
          (rnrs exceptions)
          (srfi :8 receive)
          (srfi :9 records)
          (only (srfi :13) string-index substring/shared string-map)
          (srfi :14 char-sets)
          (only (srfi :43 vectors) vector-any)
          (only (spells misc) and=>)
          (spells finite-types)
          (spells string-utils)
          (spells tracing)
          (spenet pct-coding)
          (spenet uri))

;;;; Header Fields

(define (make-http-header-field name value) (cons name value))
(define (http-header-field/name header-field) (car header-field))
(define (http-header-field/value header-field) (cdr header-field))

;;;; HTTP Versions

(define (make-http-version major minor)
  (cons major minor))

(define (http-version? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (exact-nonnegative-integer? (cdr object))))

(define (exact-nonnegative-integer? object)
  (and (integer? object)
       (exact? object)
       (>= object 0)))

(define (http-version/major version) (car version))
(define (http-version/minor version) (cdr version))

(define (http-version=? a b)
  (and (= (http-version/major a)
          (http-version/major b))
       (= (http-version/minor a)
          (http-version/minor b))))

(define (http-version<=? a b)
  (or (< (http-version/major a)
         (http-version/major b))
      (and (= (http-version/major a)
              (http-version/major b))
           (<= (http-version/minor a)
               (http-version/minor b)))))


;;;; HTTP Responses

(define-record-type <http-response>
    (%make-http-response version status-code reason header-fields body)
    http-response?
  (version http-response/version)
  (status-code http-response/status-code)
  (reason http-response/reason)
  (header-fields http-response/header-fields)
  (body http-response/body))

(define make-http-response
  (case-lambda
    ((version status-code reason header-fields body)
     (receive (status-code reason)
              (normalize-http-status+reason status-code reason)
       (%make-http-response version status-code reason header-fields body)))
    ((version status-code reason header-fields)
     (make-http-response version status-code reason header-fields #f))))

(define (normalize-http-status+reason status reason)
  (define who 'normalize-http-status-code+reason)
  (define (lose/unknown)
    (error who "unknown HTTP status, and no reason given" status))
  (define (lose/type)
    (error who "invalid type for HTTP status" status))
  (define (lookup proc)
    (let ((known-status (proc status)))
      (unless known-status
        (lose/unknown))
      (values (http-status/code known-status)
              (http-status/reason known-status))))
  (cond ((not reason)
         (cond ((integer? status)
                (lookup integer->http-status status))
               ((symbol? status)
                (lookup symbol->http-status))
               ((http-status? status)
                (lookup values))
               (else
                (lose/type))))
        ((integer? status)
         (values status reason))
        ((symbol? status)
         (values (or (and=> (symbol->http-status status)
                            http-status/code)
                     (lose/unknown))
                 reason))
        ((http-status? status)
         (values (http-status/code status) reason))
        (else
         (lose/type))))

(define (http-response/status-type response)
  (case (div (http-response/status-code response) 100)
    ((1) 'informational)
    ((2) 'success)
    ((3) 'redirection)
    ((4) 'client-error)
    ((5) 'server-error)
    (else #f)))


;;;; HTTP Requests

(define-record-type <http-request>
    (make-http-request version method uri header-fields body)
    http-request?
  (method http-request/method)
  (uri http-request/uri)
  (version http-request/version)
  (header-fields http-request/header-fields)
  (body http-request/body))

(define (http-request/uri-query-alist request)
  (cond ((uri-query (http-request/uri request))
         => www-form-urlencoded->alist)
        (else '())))

(define (pct-decode-symbol string)
  (string->symbol (string-downcase
                   (utf8->string (pct-decode string)))))

(define (www-form-urlencoded->alist str)
  (map (lambda (part)
         (let ((equal-sign (string-index part #\=)))
           (if equal-sign
               (cons (pct-decode-symbol
                      (substring/shared part 0 equal-sign))
                     (utf8->string
                      (pct-decode (string-map (lambda (c)
                                                (case c
                                                  ((#\+) #\space)
                                                  (else c)))
                                              part
                                              (+ equal-sign 1)
                                              (string-length part)))))
               (cons (pct-decode-symbol part) ""))))
       (string-split str #\&)))


;;;; Known HTTP status codes

(define-finite-type http-status <http-status>
  (code reason)
  http-status?
  http-status-vector
  http-status/name
  http-status/index
  
  (code http-status/code)
  (reason http-status/reason)
  
  (
   (ok                  200 "OK")
   (created             201 "Created")
   (accepted            202 "Accepted")
   (prov-info           203 "Provisional Information")
   (no-content          204 "No Content")

   (mult-choice         300 "Multiple Choices")
   (moved-perm          301 "Moved Permanently")
   (moved-temp          302 "Moved Temporarily")
   (method              303 "Method (obsolete)")
   (not-mod             304 "Not Modified")

   (bad-request         400 "Bad Request")
   (unauthorized        401 "Unauthorized")
   (payment-req         402 "Payment Required")
   (forbidden           403 "Forbidden")
   (not-found           404 "Not Found")
   (method-not-allowed  405 "Method Not Allowed")
   (none-acceptable     406 "None Acceptable")
   (proxy-auth-required 407 "Proxy Authentication Required")
   (timeout             408 "Request Timeout")
   (conflict            409 "Conflict")
   (gone                410 "Gone")

   (internal-error      500 "Internal Server Error")
   (not-implemented     501 "Not Implemented")
   (bad-gateway         502 "Bad Gateway")
   (service-unavailable 503 "Service Unavailable")
   (gateway-timeout     504 "Gateway Timeout")))

(define (symbol->http-status symbol)
  (vector-any (lambda (status)
                (and (eq? (http-status/name status) symbol)
                     status))
              http-status-vector))

(define (integer->http-status code)
  (vector-any (lambda (status)
                (and (= (http-status/code status) code)
                     status))
              http-status-vector))

(define-condition-type &http-error &error
  make-http-error http-error?
  (status http-error/status)
  (request http-error/request))

(define (http-error/status-code error)
  (http-status/code (http-error/status error)))

(define (http-error/reason error)
  (http-status/reason (http-error/status error)))

(define (http-error status request . message+irritants)
  (if (null? message+irritants)
      (raise (condition (make-http-error status request)))
      (raise (condition
              (make-http-error status request)
              (make-message-condition (car message+irritants))
              (make-irritants-condition (cdr message+irritants))))))

)

;; Local Variables:
;; scheme-indent-styles: ((define-record-type 3)
;;                        (finite-type-case 2)
;;                        (let-args 2)
;;                        (match 1))
;; End:
