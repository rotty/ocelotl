#!r6rs
;;; uri.scm --- Wrapper for the URI library test suite

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (spells include)
        (wak trc-testing)
        (ocelotl net uri))

(include-file/downcase ((ocelotl private) test-uri))

(run-test-suite uri-tests)
