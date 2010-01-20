;;; path-dispatch.sls --- Path dispatching

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a library intended for dispatching on URI paths, inspired
;; by the one in PLT Scheme's webserver.

;;; Code:
#!r6rs

(library (ocelotl net path-dispatch)
  (export path-dispatcher
          path-reconstructor
          path-dispatch-rules

          make-path-arg-optional

          path-arg/string path-arg/string*
          path-arg/number path-arg/number*
          path-arg/integer path-arg/integer*
          path-arg/iso-date path-arg/iso-date*)
  (import (rnrs)
          (for (only (srfi :1)
                     find-tail
                     filter-map
                     append-reverse)
               expand)
          (srfi :2 and-let*)
          (for (srfi :8 receive) expand)
          (srfi :19 time)
          (spells alist)
          (spells foof-loop)
          (for (ocelotl net path-dispatch helpers) expand))

(define (match-path-args path arg-matchers)
  (let ((not-found (list 'not-found)))
    (define (fail) not-found)
    (loop next-matcher
        ((for arg-matcher (in-list arg-matchers))
         (with args '())
         (with path path (if (null? path) path (cdr path))))
      => (and (null? path) (reverse args))
      (if (pair? arg-matcher)
          (let ((elt-matcher (car arg-matcher)))
            (define (done arg path-tail)
                (next-matcher (=> args (cons (reverse arg) args))
                              (=> path path-tail)))
            (loop next-elt ((for path-elt path-tail (in-list path))
                            (with arg '()))
              => (done arg path-tail)
              (let ((arg-elt (elt-matcher path-elt fail)))
                (if (eq? arg-elt not-found)
                    (done arg path-tail)
                    (next-elt (=> arg (cons arg-elt arg)))))))
          (let ((arg (arg-matcher (and (pair? path) (car path)) fail)))
            (if (eq? arg not-found)
                #f
                (next-matcher (=> args (cons arg args)))))))))

(define-syntax path-dispatcher
  (lambda (stx)
    (define who 'path-dispatcher)
    (define (transform-clause clause)
      (define (argument? p)
        (not (string? p)))
      (syntax-case clause (else)
        ((else proc) clause)
        (((p ...) proc)
         (receive (literals arg-matchers)
                  (split-path-pattern who #'(p ...) #f)
           (cond
             ((null? arg-matchers)
              #`((equal? path '#,literals) proc))
             (else
              #`((let ((pathtail (list-head-match '#,literals
                                                  path
                                                  #,(length literals))))
                   (and pathtail
                        (match-path-args pathtail (list #,@arg-matchers))))
                 => (lambda (path-args)
                      (lambda args
                        (apply proc (append args path-args)))))))))))
    (syntax-case stx ()
      ((_ clause ...)
       (with-syntax (((cond-clause ...) (map transform-clause #'(clause ...))))
         #`(lambda (path)
             (cond cond-clause ...)))))))

(define-syntax path-dispatch-rules
  (syntax-rules ()
    ((_ clause ...)
     (values
       (path-dispatcher clause ...)
       (path-reconstructor clause ...)))))

(define-syntax path-reconstructor
  (lambda (stx)
    (define who 'path-reconstructor)
    (define (clauses->alist clauses)
      (filter-map
       (lambda (clause)
         (syntax-case clause (else)
           (((p ...) proc)
            (receive (literals arg-unparsers)
                     (split-path-pattern who #'(p ...) #t)
              (with-syntax (((literal ...) literals)
                            ((arg-unparser ...) arg-unparsers))
                #'(,proc proc (literal ...) ,(list arg-unparser ...)))))
           ((else proc)
            #f)))
       clauses))
    (syntax-case stx ()
      ((_ clause ...)
       (with-syntax (((alist-entry ...) (clauses->alist #'(clause ...))))
         #'(let ((alist `(alist-entry ...))
                 (who 'path-reconstructor))
             (lambda (proc . args)
               (cond ((assq-ref alist proc)
                      => (lambda (entry)
                           (unparse-path who
                                         (car entry)
                                         (cadr entry)
                                         (caddr entry)
                                         args)))
                     (else
                      (error who "no matching procedure" proc))))))))))

(define (unparse-path who what prefix arg-unparsers args)
  (define (lose msg . irritants)
    (error who msg what args))
  (loop continue ((for unparser (in-list arg-unparsers))
                  (with args args)
                  (with path '()))
    => (if (null? args)
           (append prefix (reverse path))
           (lose "superfluous arguments" args))
    (cond ((null? args)
           (lose "too few arguments" args))
          ((pair? unparser)
           (continue (=> path (append-reverse
                               (map (car unparser) (car args))
                               path))
                     (=> args (cdr args))))
          (else
           (let ((path-elt (unparser (car args))))
             (continue (=> path (if path-elt (cons path-elt path) path))
                       (=> args (cdr args))))))))


;; Path argument parsers and unparsers

(define (path-arg/number unparse?)
  (if unparse?
      (lambda (n) (number->string n))
      (lambda (s fail) (or (and s (string->number s)) (fail)))))

(define (path-arg/integer unparse?)
  (if unparse?
      (lambda (n) (number->string n))
      (lambda (s fail) (or (and-let* (s
                                      (n (string->number s))
                                      ((integer? n)))
                             n)
                           (fail)))))

(define (path-arg/string unparse?)
  (if unparse?
      (lambda (s) s)
      (lambda (s fail) (or s (fail)))))

(define isodate-fmt "~Y-~m-~d")

(define (date-with-zone-offset date tz-offset)
  (make-date (date-nanosecond date)
             (date-second date)
             (date-minute date)
             (date-hour date)
             (date-day date)
             (date-month date)
             (date-year date)
             tz-offset))

(define (path-arg/iso-date unparse?)
  (if unparse?
      (lambda (date)
        (date->string date isodate-fmt))
      (lambda (s fail)
        (cond ((string=? s "today")
               (current-date 0))
              (else
               (guard (c (#t (fail)))
                 (date-with-zone-offset (string->date s isodate-fmt) 0)))))))

(define (make-path-arg-optional arg-parser)
  (let ((parse (arg-parser #f))
        (unparse (arg-parser #t)))
    (lambda (unparse?)
      (if unparse?
          (lambda (x)
            (and x (unparse x)))
          (lambda (s fail)
            (and s (parse s fail)))))))

(define path-arg/number* (make-path-arg-optional path-arg/number))
(define path-arg/integer* (make-path-arg-optional path-arg/integer))
(define path-arg/string* (make-path-arg-optional path-arg/string))
(define path-arg/iso-date* (make-path-arg-optional path-arg/iso-date))


;;;; Utilities

;; returns tail of l2
(define (list-head-match l1 l2 n)
  (cond
   ((zero? n) l2)
   ((null? l2) #f)
   ((not (equal? (car l1) (car l2))) #f)
   (else (list-head-match (cdr l1) (cdr l2) (- n 1)))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
