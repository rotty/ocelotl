;; Copyright (C) 2008-2010 Andreas Rottmann <a dot rottmann at gmx dot at>
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>

;; This file is based on SSAX's SXML-to-HTML.scm and is in the public
;; domain.
;;
;; CDATA escaping based on serializer.scm code from sxml-tools, also
;; public domain.

;;; Commentary:
;;
;; A simple interface to XML parsing and serialization.
;;
;;; Code:
#!r6rs

(library (ocelotl ssax-utils)
  (export sxml->xml
          sxml->string
          xml->sxml
          xml-cdata-escape
          universal-sxslt-rules)
  (import (rnrs)
          (only (srfi :13)
                string-index
                string-null?
                string-concatenate-reverse)
          (only (xitomatl ssax private-5-1 util)
                make-char-quotator)
          (xitomatl ssax parsing)
          (xitomatl ssax raise)
          (xitomatl ssax tree-trans))

;;@ Use SSAX to parse an XML document into SXML. The optional argument
;; @var{options} is an alist that can be used to modify the behaviour
;; of the parser; the following options are supported:
;;
;; @table @code
;; @item ns-prefixes
;; An alist with namespace prefixes to use; the @code{car} is a symbol
;; to be used as prefix, and the @code{cdr} is the URL (a string) for
;; that prefix.
;; 
;; @item entities
;; An alist mapping entity names (symbols) to their expansions (strings).
;; @end table
(define xml->sxml
  (case-lambda
    ((port)
     (xml->sxml port '()))
    ((port options)
     (let* ((ns-prefixes (option-ref options 'ns-prefixes '()))
            (namespaces
             (map (lambda (el)
                    (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
                  ns-prefixes))
           (entities (option-ref options 'entities '())))
       
       (define (res-name->sxml res-name)
         (string->symbol
          (string-append
           (symbol->string (car res-name))
           ":"
           (symbol->string (cdr res-name)))))
       
       (let ((result
              (reverse
               ((ssax:make-parser
                 NEW-LEVEL-SEED 
                 (lambda (elem-gi attributes namespaces
                                  expected-content seed)
                   '())
   
                 FINISH-ELEMENT
                 (lambda (elem-gi attributes namespaces parent-seed seed)
                   (let ((seed (ssax:reverse-collect-str-drop-ws seed))
                         (attrs
                          (attlist-fold
                           (lambda (attr accum)
                             (cons (list 
                                    (if (symbol? (car attr)) (car attr)
                                        (res-name->sxml (car attr)))
                                    (cdr attr)) accum))
                           '() attributes)))
                     (cons
                      (cons 
                       (if (symbol? elem-gi) elem-gi
                           (res-name->sxml elem-gi))
                       (if (null? attrs) seed
                           (cons (cons '^ attrs) seed)))
                      parent-seed)))

                 CHAR-DATA-HANDLER
                 (lambda (string1 string2 seed)
                   (if (string-null? string2) (cons string1 seed)
                       (cons* string2 string1 seed)))

                 DOCTYPE
                 (lambda (port docname systemid internal-subset? seed)
                   (when internal-subset?
                     (ssax:warn port
                                "Internal DTD subset is not currently handled ")
                     (ssax:skip-internal-dtd port))
                   (ssax:warn port "DOCTYPE DECL " docname " "
                              systemid " found and skipped")
                   (values #f entities namespaces seed))

                 UNDECL-ROOT
                 (lambda (elem-gi seed)
                   (values #f entities namespaces seed))

                 PI
                 ((*DEFAULT*
                   . (lambda (port pi-tag seed)
                       (cons
                        (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
                        seed)))))
                port '()))))
         (cons '*TOP*
               (if (null? ns-prefixes)
                   result
                   (cons
                    (list '^ (cons '*NAMESPACES* 
                                   (map (lambda (ns) (list (car ns) (cdr ns)))
                                        ns-prefixes)))
                    result))))))))

;; Universal transformation rules. Works for all XML.

;;@ A set of @code{pre-post-order} rules that transform any SXML tree
;; into a form suitable for XML serialization by @code{(sxml transform)}'s
;; @code{SRV:send-reply}. Used internally by @code{sxml->xml}.
(define universal-sxslt-rules
  `((^
     ((*DEFAULT* . ,(lambda (attr-key . value) ((enattr attr-key) value))))
     . ,(lambda (trigger . value) (list '^ value)))
    (*ENTITY*    . ,(lambda (tag name) (list "&" name ";")))
    ;; Is this right for entities? I don't have a reference for
    ;; public-id/system-id at the moment...
    (*DEFAULT*   . ,(lambda (tag . elems) (apply (entag tag) elems)))
    (*TEXT*      . ,(lambda (trigger str)
                      (if (string? str) (string->escaped-xml str) str)))))

;;@ Serialize the sxml tree @var{tree} as XML, writing to the textual
;; output port @var{port}.
(define (sxml->xml sxml port)
  (let ((tree (pre-post-order sxml universal-sxslt-rules)))
    (let loop ((tree tree) (result #f))
      (cond
       ((null? tree) result)
       ((not (car tree)) (loop (cdr tree) result))
       ((null? (car tree)) (loop (cdr tree) result))
       ((eq? #t (car tree)) (loop (cdr tree) #t))
       ((pair? (car tree))
        (loop (cdr tree) (loop (car tree) result)))
       ((procedure? (car tree))
        ((car tree) port)
        (loop (cdr tree) #t))
       (else
        (display (car tree) port)
        (loop (cdr tree) #t))))))

;; The following two functions serialize tags and attributes. They are
;; being used in the node handlers for the post-order function, see
;; above.

(define (check-name name)
  (let* ((str (symbol->string name))
         (i (string-index str #\:))
         (head (or (and i (substring str 0 i)) str))
         (tail (and i (substring str (+ i 1) (string-length str)))))
    (and i (string-index (substring str (+ i 1) (string-length str)) #\:)
         (parser-error "Invalid QName: more than one colon" name))
    (for-each
     (lambda (s)
       (and s
            (or (char-alphabetic? (string-ref s 0))
                (eq? (string-ref s 0) #\_)
                (parser-error "Invalid name starting character" s name))
            (string-for-each
             (lambda (c)
               (or (char-alphabetic? c) (string-index "0123456789.-_" c)
                   (parser-error "Invalid name character" c s name)))
             s)))
     (list head tail))))

(define (entag tag)
  (check-name tag)
  (lambda elems
    (if (and (pair? elems) (pair? (car elems)) (eq? '^ (caar elems)))
        (list #\< tag (cdar elems)
              (if (pair? (cdr elems))
                  (list #\> (cdr elems) "</" tag #\>)
                  " />"))
        (list #\< tag
              (if (pair? elems)
                  (list #\> elems "</" tag #\>)
                  " />")))))

(define (enattr attr-key)
  (check-name attr-key)
  (let ((attr-str (symbol->string attr-key)))
    (lambda (value)
      (list #\space attr-str
            "=\"" (and (not (null? value)) value) #\"))))

;;@ Detag an sxml tree @var{sxml} into a string. Does not perform any
;; formatting."
(define (sxml->string sxml)
  (string-concatenate-reverse
   (foldts
    (lambda (seed tree)                 ; fdown
      '())
    (lambda (seed kid-seed tree)        ; fup
      (append kid-seed seed))
    (lambda (seed tree)                 ; fhere
      (if (string? tree) (cons tree seed) seed))
    '()
    sxml)))

;; Given a string, check to make sure it does not contain characters
;; such as '<' or '&' that require encoding. Return either the original
;; string, or a list of string fragments with special characters
;; replaced by appropriate character entities.

(define string->escaped-xml
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))

;; Returns #f if a given character `ch' is in XML character range
;; Otherwise, returns a string representing the character reference for that
;; character
(define (xml-char-escaped ch)
  (let ((code (char->integer ch)))
    (if (or (= code 9) (= code 10) (= code 13)
            (and (>= code 32) (<= code 55295))
            (and (>= code 57344) (<= code 65533))
            (>= code 65536))
        #f
        (string-append "&#" (number->string code) ";"
                       ))))

;; Represents a given string `str' as a CDATA section. If @2 is given,
;; it is applied to a string that contains only characters in the XML
;; character range. The default for @2 is to return the string
;; enclosed in "<![CDATA[" and "]]>".
(define xml-cdata-escape
  (case-lambda
    ((str wrap)
     ;; If a `buffer' is non-empty, convert it to a CDATA string and
     ;; cons'es this string to `res'. Returns a new res.
     (define (flush-buffer buffer res)
       (if (null? buffer)
           res
           (cons (wrap (list->string (reverse buffer))) res)))
     (let loop ((src (string->list str))
                (buffer '())
                (res '("")))
       (cond
         ((null? src)
          (string-concatenate-reverse (flush-buffer buffer res)))
         ((xml-char-escaped (car src))
          => (lambda (charref)
               (loop (cdr src)
                     '()
                     (cons charref (flush-buffer buffer res)))))
         ((and (char=? (car src) #\])
               (not (null? buffer))
               (char=? (car buffer) #\]))
          (loop (cdr src)
                '()
                (cons (string (car buffer) (car src)) ;= "]]"
                      (flush-buffer (cdr buffer) res))))
         (else                          ; any other character
          (loop (cdr src)
                (cons (car src) buffer)
                res)))))
    ((str)
     (xml-cdata-escape str (lambda (s)
                             (string-append "<![CDATA[" s "]]>"))))))

(define (option-ref options name default)
  (cond ((assq name options) => cdr)
        (else default)))

)
