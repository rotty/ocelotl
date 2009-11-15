;;; pct-coding.scm --- Unit tests for (ocelotl net pct-coding)

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-test-suite pct-coding-tests
  "Percent encoding and decoding")

(define-test-case pct-coding-tests encode ()
  (test-equal "Hello%20World%21"
    (pct-encode (string->utf8 "Hello World!") char-set:letter+digit)))

(define-test-case pct-coding-tests decode ()
  (test-compare bytevector=? (string->utf8 "Hello World!")
    (pct-decode "Hello%20Worl%64%21")))

(run-test-suite pct-coding-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
