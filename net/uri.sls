;;; uri.sls --- URI abstraction

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (ocelotl net uri)
  (export
    make-uri
    uri?
    absolute-uri?
    relative-uri?
    uri-absolute?
    uri-relative?
    uri-scheme
    uri-authority
    uri-path
    uri-query
    uri-fragment
    uri=?

    ;; URI Authorities
    make-uri-authority
    uri-authority?
    uri-authority-userinfo
    uri-authority-host
    uri-authority-port
    uri-authority=?

    ;; URI Component Predicates
    uri-scheme?
    uri-userinfo?
    uri-host?
    uri-port?
    uri-path?
    uri-path-absolute?
    uri-path-relative?
    uri-query?
    uri-fragment?

    ;; URI Operations
    merge-uris

    ;; URI->String Conversion
    uri->string
    uri-authority->string
    write-uri
    write-uri-authority

    ;; String->URI Conversion
    object->uri
    object->absolute-uri
    object->relative-uri
    maybe-string->uri
    maybe-string->absolute-uri
    maybe-string->relative-uri
    string->uri
    string->absolute-uri
    string->relative-uri

    ;; String->URI-Path Conversion
    string->uri-path
    maybe-string->uri-path
    )
  (import (rename (except (rnrs base)
                          string-copy
                          string->list
                          string-for-each)
                  (error rnrs:error))
          (rnrs unicode)
          (rnrs control)
          (rnrs hashtables)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs r5rs)
          (except (srfi :1 lists) for-each map)
          (srfi :9 records)
          (only (srfi :13 strings) string-downcase!)
          (srfi :14 char-sets)
          (spells ascii)
          (wak foof-loop)
          (spells gc)
          (only (spells error) make-error-signaller)
          (spells include)
          (wak parscheme matcher-combinators)
          (wak parscheme text-matcher-combinators)
          (wak parscheme parser-combinators)
          (wak parscheme text-parser-combinators)
          (wak parscheme parse-errors))

    ;++ This is bogus.  Also, `internment camp' might, just might, not
    ;++ be quite the right name, but it works for now.
    (define (make-string-internment-camp)
      (make-hashtable string-hash string=?))
    (define (intern internment-camp string generator)
      (define (generate)
        (let ((datum (generator)))
          (hashtable-set! internment-camp string (make-weak-cell datum))
          datum))
      (cond ((hashtable-ref internment-camp string #f)
             => (lambda (weak-cell)
                  (or (weak-cell-ref weak-cell)
                      (generate))))
            (else (generate))))
    (define (soft-intern internment-camp string)
      (cond ((hashtable-ref internment-camp string #f)
             => weak-cell-ref)
            (else #f)))

    (define (error message object context)
      (rnrs:error context message object))
    
    (define (write-string s port)
      (put-string port s))
    
    (include-file/downcase ((ocelotl net private) uri))
  )
