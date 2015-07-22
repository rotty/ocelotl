#!r6rs
;;; parser-utils.sls --- Parscheme utilities

;; Copyright (C) 2009, 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (ocelotl parser-utils)
  (export parse-input-bytes-as-latin1)
  (import (rnrs)
          (srfi :45 lazy)
          (wak riastreams)
          (wak parscheme parser-combinators))

(define (parse-input-bytes-as-latin1 parser input-port context win lose)
  (parse-stream parser
                (let recur ()
                  (lazy (let ((byte (get-u8 input-port)))
                          (if (eof-object? byte)
                              stream-nil
                              (stream-cons (integer->char byte) (recur))))))
                (cons 1 1)
                (lambda (position char)
                  (let ((line (car position))
                        (column (cdr position)))
                    (if (char=? char #\newline)
                        (cons (+ line 1) 1)
                        (cons line (+ column 1)))))
                context
                win
                lose))

)
