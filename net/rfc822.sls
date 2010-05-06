;;; rfc822.sls --- RFC822-style header parsing

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Based on Public Domain code written by Taylor R. Campbell.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (ocelotl net rfc822)
  (export read-rfc822-headers
          read-rfc822-headers-with-line-breaks
          time-utc->rfc822-string
          rfc822-malformed-headers-condition?)
  (import (rnrs)
          (only (srfi :13)
                string-join
                string-trim-right)
          (srfi :14 char-sets)
          (srfi :19 time)
          (spells ascii)
          (wak parscheme parse-errors)
          (wak parscheme parser-combinators)
          (wak parscheme text-parser-combinators)
          (ocelotl parser-utils))

(define-condition-type &rfc822-malformed-headers &error
  make-rfc822-malformed-headers-condition
  rfc822-malformed-headers-condition?)

(define (raise-rfc822-malformed-headers who perror)
  (raise (condition
          (make-rfc822-malformed-headers-condition)
          (make-who-condition who)
          (make-message-condition "Malformed RFC822 headers")
          (make-irritants-condition
           (list (parse-error/position perror)
                 (parse-error/messages perror))))))

(define (make-rfc822-header-reader who parser)
  (lambda (port)
    (let ((parse-input (if (textual-port? port)
                           parse-input-chars
                           parse-input-bytes-as-latin1)))
      (parse-input parser
                   port
                   #f
                   (lambda (headers context stream)
                     headers)
                   (lambda (perror context stream)
                     (raise-rfc822-malformed-headers who perror))))))

(define rfc822-date-fmt "~a, ~d ~b ~Y ~H:~M:~S GMT")

(define (time-utc->rfc822-string time)
  (date->string (time-utc->date time 0) rfc822-date-fmt))


;;;;;; Parsing Header Fields

(define-parser (rfc822-parser:header-fields transform-value)
  (parser:list:repeated-until
      (parser:choice rfc822-parser:crlf (parser:end))
    (rfc822-parser:header-field transform-value)))

(define-parser (rfc822-parser:header-field transform-value)
  (*parser
      (name rfc822-parser:header-field-name)
      (value rfc822-parser:header-field-value)
    (parser:return
     (cons name (transform-value value)))))

(define-parser rfc822-parser:header-field-name
  (parser:map (lambda (s)
                (string->symbol (string-downcase s)))
    (parser:string:at-least-until 1 (parser:char= #\:)
      (parser:char-in-set rfc822-char-set:header-field-name))))

(define-parser rfc822-parser:header-field-value
  (*parser
      (initial-line rfc822-parser:header-field-initial-line)
      (continuation-lines
       (parser:list:repeated rfc822-parser:header-field-continuation-line))
    (parser:return
     (cons initial-line continuation-lines))))

(define-parser rfc822-parser:header-field-initial-line
  (parser:sequence rfc822-parser:lws* rfc822-parser:header-field-line))

(define-parser rfc822-parser:header-field-continuation-line
  (parser:sequence rfc822-parser:lws+ rfc822-parser:header-field-line))

(define-parser rfc822-parser:header-field-line
  (parser:map string-trim-right
    (parser:string:repeated-until rfc822-parser:crlf
      (parser:char-in-set rfc822-char-set:header-field-value))))


;;;;;; Parsing Utilities and Character Sets

(define rfc822-char-set:lws             ;Linear White Space
  (char-set (ascii->char #x09)          ;  TAB
            (ascii->char #x20)))        ;  SPC

(define-parser rfc822-parser:lws
  (parser:char-in-set rfc822-char-set:lws))

(define-parser rfc822-parser:lws*
  (parser:noise:repeated rfc822-parser:lws))

(define-parser rfc822-parser:lws+
  (parser:noise:at-least 1 rfc822-parser:lws))

(define rfc822-char-set:crlf
  (char-set (ascii->char #x0D)
            (ascii->char #x0A)))

(define-parser rfc822-parser:crlf
  (let ((carriage-return (parser:char= (ascii->char #x0D)))
        (line-feed (parser:char= (ascii->char #x0A))))
    (parser:choice
     (parser:sequence carriage-return
                      (parser:optional-noise line-feed))
     line-feed)))

(define rfc822-char-set:header-field-name
  (char-set-complement (char-set-adjoin rfc822-char-set:crlf #\:)))

(define rfc822-char-set:header-field-value
  (char-set-union (char-set-complement char-set:iso-control)
                  rfc822-char-set:lws))

;;;; Utilities

(define read-rfc822-headers
  (make-rfc822-header-reader 'read-rfc822-headers
                             (rfc822-parser:header-fields
                              (lambda (lines)
                                (string-join lines " ")))))

(define read-rfc822-headers-with-line-breaks
  (make-rfc822-header-reader 'read-rfc822-headers
                             (rfc822-parser:header-fields
                              (lambda (lines)
                                lines))))

)

;; Local Variables:
;; scheme-indent-styles: (parscheme)
;; End:
