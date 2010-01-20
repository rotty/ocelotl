;;; make-options.sls --- Construct HTTPd options

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (ocelotl net httpd make-options)
  (export make-httpd-options
          with-port
          with-interface
          with-request-handler
          with-server-header
          with-debug?)
  (import (rnrs)
          (spells syntax-utils)
          (ocelotl net httpd options))

(define (httpd-options-transformer replacer)
  (case-lambda
    ((new-value options)
     (replacer options new-value))
    ((new-value)
     (httpd-options-transformer new-value (make-default-httpd-options)))))

(define-syntax define-httpd-options-transformers
  (lambda (stx)
    (syntax-case stx ()
      ((k field ...)
       (let ()
         (define (field-name->modifier-name field-name)
           (identifier-append #'k 'httpd-options-with- (syntax->datum field-name)))
         (define (field-name->transformer-name field-name)
           (identifier-append #'k 'with- (syntax->datum field-name)))
         (with-syntax (((modifier-name ...)
                        (map field-name->modifier-name #'(field ...)))
                       ((transformer-name ...)
                        (map field-name->transformer-name #'(field ...))))
           #'(begin
               (define transformer-name
                 (httpd-options-transformer modifier-name))
               ...)))))))

(define-httpd-options-transformers
  port
  interface
  request-handler
  server-header
  debug?)

(define (make-httpd-options . plist)
  (let loop ((options (make-default-httpd-options))
             (plist plist))
    (if (null? plist)
        options
        (loop ((car plist) (cadr plist) options)
              (cddr plist)))))

)
