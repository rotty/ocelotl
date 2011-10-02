;;; simple-httpd.sls --- Simple HTTP server

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the WTFPL, version 2, as 
;; published by Sam Hocevar. See http://sam.zoy.org/wtfpl/COPYING
;; for more details.

;;; Commentary:

;; This program runs a simple HTTP server serving static content.

;;; Code:
#!r6rs

(import (rnrs)
        (ocelotl net soup-httpd)
        (ocelotl net httpd options)
        (ocelotl net httpd file-directory-handlers))

(httpd (make-httpd-options
        with-port 8181
        with-request-handler (rooted-file-or-directory-handler ".")))
