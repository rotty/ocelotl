;;; -*- Mode: Scheme; scheme48-package: uri-tests -*-

;;;; Schemantic Web
;;;; URI Tests

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-test-suite uri-tests
  "URI: Uniform Resource Identifiers")

(define-syntax test-uri=
  (syntax-rules ()
    ((TEST-URI= expected-expression actual-expression)
     (TEST-COMPARE URI=? expected-expression actual-expression))))

(define-test-case uri-tests parsing.0 ()
  (test-uri= (make-uri 'SCHEME
                       (make-uri-authority "user" "host" 123)
                       '("" "path" "to" "file")
                       "query"
                       "fragment")
    (string->uri "scheme://user@host:123/path/to/file?query#fragment")))

;;;; Merging

(define-test-suite (uri-tests.merging uri-tests)
  "Relative URI merging rules")

(let-syntax
    ((define-uri-merging-tests
      (syntax-rules ()
        ((DEFINE-URI-MERGE-TESTS name base-uri
           (relative-reference expected-result)
           ...)
         (DEFINE-TEST-CASE URI-TESTS.MERGING name ()
           (TEST-EQUAL expected-result
             (URI->STRING (MERGE-URIS relative-reference base-uri #T)))
           ...)))))
  (define-uri-merging-tests normal-examples "http://a/b/c/d;p?q"
    ("g:h"      "g:h")
    ("g"        "http://a/b/c/g")
    ("./g"      "http://a/b/c/g")
    ("g/"       "http://a/b/c/g/")
    ("/g"       "http://a/g")
    ("//g"      "http://g")
    ("?y"       "http://a/b/c/d;p?y")
    ("g?y"      "http://a/b/c/g?y")
    ("#s"       "http://a/b/c/d;p?q#s")
    ("g#s"      "http://a/b/c/g#s")
    ("g?y#s"    "http://a/b/c/g?y#s")
    (";x"       "http://a/b/c/;x")
    ("g;x"      "http://a/b/c/g;x")
    ("g;x?y#s"  "http://a/b/c/g;x?y#s")
    (""         "http://a/b/c/d;p?q")
    ("."        "http://a/b/c/")
    ("./"       "http://a/b/c/")
    (".."       "http://a/b/")
    ("../"      "http://a/b/")
    ("../g"     "http://a/b/g")
    ("../.."    "http://a/")
    ("../../"   "http://a/")
    ("../../g"  "http://a/g")
    )
  (define-uri-merging-tests abnormal-examples "http://a/b/c/d;p?q"
    ("../../../g"       "http://a/g")
    ("../../../../g"    "http://a/g")
    ("/./g"             "http://a/g")
    ("/../g"            "http://a/g")
    ("g."               "http://a/b/c/g.")
    (".g"               "http://a/b/c/.g")
    ("g.."              "http://a/b/c/g..")
    ("..g"              "http://a/b/c/..g")
    ("./../g"           "http://a/b/g")
    ("./g/."            "http://a/b/c/g/")
    ("g/./h"            "http://a/b/c/g/h")
    ("g/../h"           "http://a/b/c/h")
    ("g;x=1/./y"        "http://a/b/c/g;x=1/y")
    ("g;x=1/../y"       "http://a/b/c/y")
    ("g?y/./x"          "http://a/b/c/g?y/./x")
    ("g?y/../x"         "http://a/b/c/g?y/../x")
    ("g#s/./x"          "http://a/b/c/g#s/./x")
    ("g#s/../x"         "http://a/b/c/g#s/../x")
    ("http:g"           "http:g")
    ))
