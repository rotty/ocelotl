#!r6rs
;;; utils.sls --- ocelotl internal utilities 

;; Copyright (C) 2009, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (ocelotl private utils)
  (export make-fmt-log
          uri-with-directory-path)
  (import (rnrs)
          (only (srfi :1) last)
          (wak fmt)
          (spells logging)
          (spells string-utils)
          (ocelotl net uri))

(define (make-fmt-log logger)
  (let ((log (make-log logger)))
    (lambda (level . fmts)
      (log level
           (lambda (port)
             (fmt port (apply-cat fmts)))))))

(define (uri-with-directory-path uri)
  (let ((path (uri-path uri)))
    (make-uri (uri-scheme uri)
              (uri-authority uri)
              (if (null? path)
                  '("")
                  (let ((last-elt (last path)))
                    (if (string=? last-elt "")
                        path
                        (append path (list "")))))
              (uri-query uri)
              (uri-fragment uri))))

)
