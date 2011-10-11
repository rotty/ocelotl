#!r6rs
;;; client-context.sls --- HTTPd client context data type

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (ocelotl net httpd client-context)
  (export make-httpd-client-context
          httpd-client-context?
          httpd-client-context/host)
  (import (rnrs base)
          (srfi :9 records))

(define-record-type <httpd-client-context>
    (make-httpd-client-context host)
    httpd-client-context?
  (host httpd-client-context/host))

)
