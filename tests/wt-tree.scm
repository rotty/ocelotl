;;; wt-tree.scm --- Testcases for the weight-balanced tree library

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>
;;
;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;     2006, 2007, 2008 Massachusetts Institute of Technology
;;
;; Copyright (c) 1993-1994 Stephen Adams

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

;; Test code, using maps from digit strings to the numbers they represent.

;;; Code:


;;; Helper functions
(define (make-map lo hi step)
  (let loop ((i lo) (map (make-wt-tree string-wt-type)))
    (if (> i hi)
        map
        (loop (+ i step) (wt-tree/add map (number->string i) i)))))

(define t1 (make-map 0 99 2))   ; 0,2,4,...,98
(define t2 (make-map 1 100 2))  ; 1,3,5,...,99
(define t3 (make-map 0 100 3))  ; 0,3,6,...,99

(define (wt-tree->alist t)
  (wt-tree/fold (lambda (k d r) (cons (cons k d) r)) '() t))

(define (try-all operation trees)
  (map (lambda (t1)
         (map (lambda (t2)
                (operation t1 t2))
              trees))
       trees))


;;; The test suite

(define-test-suite wt-tree-tests
  "Weight-balanced trees")

(define-test-case wt-tree-tests fold ()
  (test-equal
      '(("0" . 0) ("12" . 12) ("15" . 15) ("18" . 18)
        ("21" . 21) ("24" . 24) ("27" . 27)
        ("3" . 3) ("30" . 30) ("33" . 33) ("36" . 36) ("39" . 39)
        ("42" . 42) ("45" . 45) ("48" . 48)
        ("51" . 51) ("54" . 54) ("57" . 57)
        ("6" . 6) ("60" . 60) ("63" . 63) ("66" . 66) ("69" . 69)
        ("72" . 72) ("75" . 75) ("78" . 78)
        ("81" . 81) ("84" . 84) ("87" . 87)
        ("9" . 9) ("90" . 90) ("93" . 93) ("96" . 96) ("99" . 99))
    (wt-tree->alist t3)))

(define-test-case wt-tree-tests union ()
  (test-equal '((50 100 67) (100 50 67) (67 67 34))
   (try-all (lambda (t1 t2) (wt-tree/size (wt-tree/union t1 t2)))
            (list t1 t2 t3))))

(define-test-case wt-tree-tests difference ()
  (test-equal '((0 50 33) (50 0 33) (17 17 0))
    (try-all (lambda (t1 t2) (wt-tree/size (wt-tree/difference t1 t2)))
             (list t1 t2 t3))))

(define-test-case wt-tree-tests intersection ()
  (test-equal '((50 0 17) (0 50 17) (17 17 34))
    (try-all (lambda (t1 t2) (wt-tree/size (wt-tree/intersection t1 t2)))
             (list t1 t2 t3))))

(define-test-case wt-tree-tests set-equal? ()
  (test-equal '((#t #f #f) (#f #t #f) (#f #f #t))
    (try-all (lambda (t1 t2) (wt-tree/set-equal? (wt-tree/difference t1 t2)
                                                   (wt-tree/difference t2 t1)))
             (list t1 t2 t3))))

(run-test-suite wt-tree-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
