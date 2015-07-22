#!r6rs
;;; options.sls --- HTTPd option data type

;; Copyright (C) 2009, 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (ocelotl net httpd options)
  (export make-default-httpd-options

          httpd-options-port
          httpd-options-interface
          httpd-options-request-handler
          httpd-options-server-header
          httpd-options-debug?
          
          httpd-options-with-port
          httpd-options-with-interface
          httpd-options-with-request-handler
          httpd-options-with-server-header
          httpd-options-with-debug?)
  (import (rnrs base)
          (spells record-types))

(define-record-type* httpd-options
  (make-httpd-options port interface request-handler server-header debug?)
  ())

(define (make-default-httpd-options)
  (make-httpd-options 80          ; port
                      "127.0.0.1" ; interface
                      #f          ; request-handler
                      #f          ; server-header
                      #f          ; debug?
                      ))

(define-functional-fields httpd-options
  port interface request-handler server-header debug?)


)
