;;; pct-coding.sls --- Percent encoding and decoding

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (ocelotl net pct-coding)
  (export pct-decode
          pct-encode
          pct-coding-error?)
  (import (rnrs)
          (srfi :2 and-let*)
          (only (srfi :13) string-concatenate)
          (srfi :14 char-sets)
          (spells ascii)
          (spells foof-loop)
          (parscheme parse-errors)
          (parscheme parser-combinators)
          (parscheme text-parser-combinators)
          (ocelotl parser-utils))

(define-condition-type &pct-coding &error
  make-pct-coding-error pct-coding-error?)

(define (raise-pct-coding-error who perror)
  (raise (condition
          (make-pct-coding-error)
          (make-who-condition who)
          (make-message-condition "Malformed percent encoding")
          (make-irritants-condition
           (list (parse-error/position perror)
                 (parse-error/messages perror))))))

;;@ Percent-decode the string @1. All characters not in the char-set
;; @2 must be in percent-encoded form in @1. @2 defaults to the ASCII
;; range, except for @code{#\%}. On success, the decoded bytevector is
;; returned, otherwise an error is signalled.
(define pct-decode
  (case-lambda
    ((string char-set)
     (parse-string (pct-parser char-set)
                   string
                   #f ; No context
                   (lambda (octets context stream)
                     (u8-list->bytevector octets))
                   (lambda (perror context stream)
                     (raise-pct-coding-error 'pct-decode perror))))
    ((string)
     (pct-decode string pct-coding-default-char-set))))

;;@ Percent-encode the bytevector @1. All non-ASCII characters, and
;; those not contained in the char-set @2 will be percent-encoded. The
;; resulting string is returned.
(define pct-encode
  (case-lambda
    ((bytevector char-set)
     (loop continue ((with parts '())
                     (with i (- (bytevector-length bytevector) 1) (- i 1))
                     (until (< i 0)))
       => (string-concatenate parts)
       (let ((octet (bytevector-u8-ref bytevector i)))
         (continue (cons
                    (or (and-let* (((< -1 octet ascii-limit))
                                   (c (ascii->char octet))
                                   ((char-set-contains? char-set c)))
                          (string c))
                        (octet->pct-string octet))
                    parts)))))
    ((bytevector)
     (pct-encode bytevector pct-coding-default-char-set))))

(define hex-digits "0123456789abcdef")

(define (octet->pct-string octet)
  (let ((a (div octet #x10))
        (b (mod octet #x10)))
    (string #\% (string-ref hex-digits a) (string-ref hex-digits b))))

(define pct-coding-default-char-set
  (char-set-difference char-set:ascii (char-set #\%)))

(define-parser (pct-parser char-set)
  (parser:list:repeated
   (parser:choice (pct-parser:octet-in-char-set char-set)
                  pct-parser:encoded-octet)))

(define-parser (pct-parser:octet-in-char-set char-set)
  (*parser
      (c (parser:char-in-set char-set))
    (parser:return (char->ascii c))))

(define-parser pct-parser:encoded-octet
  (*parser
      ((parser:char= #\%))
      (a pct-parser:hex-digit)
      (b pct-parser:hex-digit)
    (parser:return (+ (* a #x10) b))))

(define (char->hex-digit char)
  (let ((code (char->ascii char)))
    (cond ((char-numeric?    char) (- code (char->ascii #\0)))
          ((char-upper-case? char) (- (+ code #xA) (char->ascii #\A)))
          ((char-lower-case? char) (- (+ code #xA) (char->ascii #\a)))
          (else (assertion-violation 'char->hex-digit
                                     "Invalid hex digit character" char)))))

(define-parser pct-parser:hex-digit
  (*parser (char (parser:char-in-set char-set:hex-digit))
    (parser:return (char->hex-digit char))))

(define-parser pct-parser:hex-char
  (parser:char-in-set char-set:hex-digit))

)

;; Local Variables:
;; scheme-indent-styles: (parscheme foof-loop)
;; End:
