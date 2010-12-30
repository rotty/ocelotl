#!r6rs
;;; helpers.sls --- path dispatcher helpers

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

;;; Code:


(library (ocelotl net path-dispatch helpers)
  (export split-path-pattern)
  (import (for (rnrs) run (meta -1))
          (only (srfi :1) span))

(define (split-path-pattern who pat unparser?)
  (span string?
        (map
         (lambda (p)
           (syntax-case p (*)
             ((p) #`(p #,unparser?))
             ((* (p)) #`(list (p #,unparser?)))
             (p
              (let ((v (syntax->datum #'p)))
                (cond ((symbol? v) (symbol->string v))
                      ((string? v) v)
                      (else
                       (syntax-violation who
                                         "invalid path pattern"
                                         #'pat #'p)))))))
         pat)))

)
