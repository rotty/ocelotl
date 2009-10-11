;;; http-client.sps --- Simple HTTP client demo

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Usage: http-client.sps HTTP-URL
;; 
;; Performs a GET request on HTTP-URL, and copies the response body to
;; the standard output port.

;;; Code:
#!r6rs

(import (rnrs)
        (spells ports)
        (ocelotl net http-client))

(define (main argv)
  (call-with-http-response 'GET (cadr argv) '() ""
    (lambda (response response-port)
      (call-with-port (standard-output-port)
        (lambda (oport)
          (copy-port response-port oport))))))

(main (command-line))
