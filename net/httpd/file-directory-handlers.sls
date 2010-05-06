;;; file-directory-handlers.sls --- 

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; This file is based on code from the Scheme Untergrund Networking
;; package,
;; Copyright (c) 1994 by Brian D. Carlstrom and Olin Shivers.
;; Copyright (c) 1996-2003 by Mike Sperber.
;; See COPYING.BSD for the original licensing conditions.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This code is quite stat-heavy, due to a lack of a direct interface
;; to `stat(2)'. I should add such API to (spells filesys).

;;; Code:
#!r6rs

(library (ocelotl net httpd file-directory-handlers)
  (export rooted-file-handler
          rooted-file-or-directory-handler)
  (import (except (rnrs)
                  define-record-type
                  file-exists? delete-file)
          (only (srfi :13) string-join)
          (wak foof-loop)
          (spells filesys)
          (spells record-types)
          (spells pathname)
          (spells ports)
          (spells opt-args)
          (spells tracing)
          (ocelotl private utils)
          (ocelotl net uri)
          (ocelotl net rfc822)
          (ocelotl net http)
          (ocelotl net httpd)
          (ocelotl net httpd responses))


;;; Public API

(define-record-type* file-directory-options
  (make-file-directory-options pathname->content-type)
  ())

(define (make-default-file-directory-options)
  (make-file-directory-options default-pathname->content-type))

(define-functional-fields file-directory-options
  pathname->content-type)

;;; Make a handler that serves files relative to a particular root
;;; in the file system. You may follow symlinks, but you can't back up
;;; past ROOT with ..'s.

(define (rooted-file-handler root-dir . maybe-options)
  (let-optionals maybe-options ((options (make-default-file-directory-options)))
    (let ((root (pathname-as-directory root-dir)))
      (lambda (path req)
        (make-rooted-file-path-response root path file-serve-response req options)))))

;;; Dito, but also serve directory indices for directories without
;;; index.html.

(define (rooted-file-or-directory-handler root . maybe-options)
  (let-optionals maybe-options ((options (make-default-file-directory-options)))
    (lambda (path req)
      (make-rooted-file-path-response root path
                                      file-serve-and-dir-response
                                      req
                                      options))))


;;; Response generators

;; (MAKE-ROOTED-FILE-PATH-RESPONSE root file-path req options)

;; Do a request for a file. The file-name is determined by appending the
;; the FILE-PATH list the string ROOT. E.g., if
;;     ROOT = "/usr/shivers"    FILE-PATH = ("a" "b" "c" "foo.html")
;; then we serve file
;;     /usr/shivers/a/b/c/foo.html
;; Elements of FILE-PATH are *not allowed* to contain .. elements.
;;   (N.B.: Although the ..'s can appear in relative URI's, /foo/../ path 
;;    sequences are processed away by the browser when the URI is converted
;;    to an absolute URI before it is sent off to the server.)
;; It is possible to sneak a .. past this kind of front-end resolving by
;; encoding it (e.g., "foo%2F%2E%2E" for "foo/.."). If the client tries
;; this, MAKE-ROOTED-FILE-PATH-RESPONSE will catch it, and abort the transaction.
;; So you cannot make the reference back up past ROOT. E.g., this is
;; not allowed:
;;     FILE-PATH = ("a" "../.." "c" "foo.html")
;;
;; Only GET and HEAD ops are provided. 
;; The URL's <search> component must be #f.
;; The file is served if the server has read or stat(2) access to it,
;; respectively. If the server is run as root, this might be a problem.
;;
;; FILE-SERVE is a procedure which gets passed the file name, the
;; path, and the HTTP request to serve the file propert after the
;; security checks.  Look in ROOTED-FILE-HANDLER and
;; ROOTED-FILE-OR-DIRECTORY-HANDLER for examples on how to feed this.
(define (make-rooted-file-path-response root path file-serve-response req options)
  (if (uri-query (http-request/uri req))
      (make-error-response (http-status bad-request) req
                           "Indexed search not provided for this URL.")
      (cond ((path->pathname path root) =>
             (lambda (pathname)
               (file-serve-response pathname path req options)))
            (else
             (make-error-response (http-status bad-request) req
                                  "URL contains unresolvable ..'s.")))))

(define (file-serve-response pathname path req options)
  (file-serve-or-dir-response pathname path req
                              dir-index-serve-response
                              options))

(define (file-serve-or-dir-response pathname path request dir-serve-response options)
  (guard (c ((i/o-file-does-not-exist-error? c)
             (http-error (http-status not-found) request))
            ((i/o-file-protection-error? c)
             (http-error (http-status forbidden) request)))
    (if (pathname-file pathname)
        (let ((method (http-request/method request)))
          (cond ((or (string=? method "GET")
                     (string=? method "HEAD"))
                 (case (file-kind pathname request)
                   ((regular)
                    (send-file-response pathname request options))
                   ((directory)
                    (make-error-response
                     (http-status moved-perm)
                     request
                     (uri-with-directory-path (http-request/uri request))))
                   (else
                    (make-error-response (http-status forbidden) request))))
                (else
                 (make-error-response (http-status method-not-allowed) request))))
        (dir-serve-response pathname path request options))))

(define (directory-serve-response pathname path req options)
  (let ((request-method (http-request/method req)))
    (cond
     ((or (string=? request-method "GET")
          (string=? request-method "HEAD"))
      (if (not (file-directory? pathname))
          (make-error-response (http-status forbidden) req)
          (make-http-response
           (http-request/version req)
           (http-status ok)
           #f
           '((content-type . "text/html; charset=utf-8"))
           (lambda (iport oport httpd)
             (put-directory-index oport req pathname path options)))))
     (else
      (make-error-response (http-status method-not-allowed)
                           req
                           request-method)))))

(define (put-directory-index port req pathname path options)
  (let ((title (string-append "Index of /" (string-join path "/"))))
    (put-sxml-response-body
     port
     xhtml-doctype
     `(html
       (head
        (title ,title))
       (body
        (h1 ,title)
        (table
         (^ (summary "Directory Listing"))
         (thead
          (th "Name") (th "Last modified") (th "Size") (th "Type"))
         (tbody
          ,@(if (equal? path '(""))
                '()
                '((tr (td (a (^ href "..") "Parent directory"))
                      (td)
                      (td)
                      (td "Directory"))))
          ,@(map (lambda (pathname)
                   (pathname-shtml-tr pathname options))
                 (list-sort pathname-file<?
                            (directory-fold pathname cons '()))))))))))

(define (pathname-file<? pn1 pn2)
  (string<? (file-namestring pn1)
            (file-namestring pn2)))

(define (pathname-shtml-tr pathname options)
  (let* ((dir? (file-directory? pathname))
         (fname (file-namestring pathname))
         (content-type
          (if dir?
              "Directory"
              ((file-directory-options-pathname->content-type options)
               pathname))))
    `(tr (td (a (^ (href ,fname)) ,fname))
         (td ,(time-utc->rfc822-string (file-modification-time pathname)))
         (td ,(if dir?
                  ""
                  (file-size->string (file-size-in-bytes pathname))))
         (td ,content-type))))

(define (file-size->string size)
  (loop continue ((for abbrev (in-list '("GiB" "MiB" "KiB")))
                  (with scale 3 (- scale 1)))
    => (number->string size)
    (let ((n (expt 1024 scale)))
      (cond ((> size (* 10 n))
             (string-append (number->string (div size n)) " " abbrev))
            (else
             (continue))))))

(define (dir-index-serve-response pathname path req options)
  (file-serve-response (pathname-with-file pathname '("index" "html"))
                       path req options))

(define (index-or-directory-serve-response pathname path req options)
  (let ((index-pathname (pathname-with-file pathname '("index" "html"))))
    (if (and (file-exists? index-pathname)
             (file-readable? index-pathname))
        (file-serve-response index-pathname path req options)
        (directory-serve-response pathname path req options))))

(define (file-serve-and-dir-response fname file-path req options)
  (file-serve-or-dir-response fname file-path req
                              index-or-directory-serve-response
                              options))

(define (send-file-response pathname req options)
  (let ((content-type
         ((file-directory-options-pathname->content-type options) pathname))
        (file-size (file-size-in-bytes pathname)))
    (make-http-response
     (http-request/version req)
     (http-status ok)
     #f
     (append (list
              (cons 'last-modified
                    (time-utc->rfc822-string
                     (file-modification-time pathname)))
              (cons 'content-type content-type)
              (cons 'content-length (number->string file-size))))
     (lambda (iport oport httpd)
       (call-with-port (open-file-input-port (x->namestring pathname))
         (lambda (in)
           (copy-port in oport
                      (min 256000 (max (div file-size 100) 4000))
                      (lambda () (httpd/yield httpd #t)))))))))


;;; Default options

(define known-file-mime-types
  '(("text/html"                "htm" "html")
    ("text/plain"               "txt")
    ("text/css"                 "css")
    ("application/msword"       "doc")
    ("image/gif"                "gif")
    ("image/png"                "png")
    ("image/bmp"                "bmp")
    ("image/jpeg"               "jpg" "jpeg")
    ("image/tif"                "tiff" "tif")
    ("text/rtf"                 "rtf")
    ("video/mpeg"               "mpeg" "mpg")
    ("audio/basic"              "au" "snd")
    ("audio/x-wav"              "wav")
    ("application/x-dvi"        "dvi")
    ("application/latex"        "tex" "latex")
    ("application/zip"          "zip")
    ("application/tar"          "tar")
    ("application/mac-binhex40" "hqx")
    ("application/postscript"   "ps")
    ("application/pdf"          "pdf")))

(define (lookup-known-file-mime-type type)
  (exists (lambda (entry)
            (and (member type (cdr entry))
                 (car entry)))
          known-file-mime-types))

(define (default-pathname->content-type pathname)
  (let ((type (file-type (pathname-file pathname))))
    (or (and type (lookup-known-file-mime-type (string-downcase type)))
        "application/octet-stream")))


;;; Utilities

(define (file-kind pathname req)
  (if (file-exists? pathname)
    (cond ((file-regular? pathname)        'regular)
          ((file-symbolic-link? pathname)  'symbolic-link)
          ((file-directory? pathname)      'directory)
          (else                            'unknown))
    (http-error (http-status not-found) req)))

;;@ Convert @1, an URI-path into a pathname rooted in @2.
;;
;; If the path would break out of @2, @code{#f} is returned.
(define (path->pathname path root)
  (loop continue ((with comps '())
                  (with is-directory? #f)
                  (for elt path-rest (in-list path)))
    => (pathname-join root
                      (if is-directory?
                          (make-pathname #f (reverse comps) #f)
                          (make-pathname #f
                                         (reverse (cdr comps))
                                         (x->file (car comps)))))
    (cond ((string=? elt ".")
           (continue (=> is-directory? #t)))
          ((string=? elt "..")
           (if (null? comps)
               #f
               (continue (cdr comps) #t)))
          ((string=? elt "")
           (if (null? (cdr path-rest))
               (continue (=> is-directory? #t))
               (continue '() #t)))
          (else
           (continue (cons elt comps) #f)))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
