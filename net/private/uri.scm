;;; -*- Mode: Scheme; scheme48-package: uris -*-

;;;; Schemantic Web
;;;; Uniform Resource Identifiers (RFC 3986)

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <uri>
    (%%make-uri scheme authority path query fragment string hash)
    uri?
  (scheme %uri-scheme)
  (authority %uri-authority)
  (path %uri-path)
  (query %uri-query)
  (fragment %uri-fragment)
  (string %uri-string)
  (hash %uri-hash))

(define uri-camp (make-string-internment-camp))

(define (%make-uri scheme authority path query fragment string)
  (intern uri-camp string
    (lambda ()
      (%%make-uri scheme authority path query fragment string
                  (string-hash string)))))

(define (make-uri scheme authority path query fragment)
  (let ((scheme (and scheme (guarantee-uri-scheme scheme 'MAKE-URI)))
        (authority
         (and authority (guarantee-uri-authority authority 'MAKE-URI)))
        (path (guarantee-uri-path path 'MAKE-URI))
        (query (and query (guarantee-uri-query query 'MAKE-URI)))
        (fragment (and fragment (guarantee-uri-fragment fragment 'MAKE-URI))))
    (%make-uri scheme
               authority
               (if scheme (remove-dot-segments path) path)
               query
               fragment
               (uri-components->string scheme authority path query fragment))))

(define (uri-scheme uri)
  (%uri-scheme (guarantee-uri uri 'URI-SCHEME)))

(define (uri-authority uri)
  (%uri-authority (guarantee-uri uri 'URI-AUTHORITY)))

(define (uri-path uri)
  (%uri-path (guarantee-uri uri 'URI-PATH)))

(define (uri-query uri)
  (%uri-query (guarantee-uri uri 'URI-QUERY)))

(define (uri-fragment uri)
  (%uri-fragment (guarantee-uri uri 'URI-FRAGMENT)))

;;; If the argument to URI-HASH is a string, we absolutely cannot just
;;; match it against the URI matcher and then get its hash without
;;; parsing it into its components and then computing the hash.  This
;;; is because we must canonicalize the percent-encoding, or equivalent
;;; URI strings would be hashed differently, which is a Bad Thing for
;;; security.

(define (uri-hash uri)
  (%uri-hash (guarantee-uri uri 'URI-HASH)))

(define (uri=? a b)
  (eq? (guarantee-uri a 'URI=?)
       (guarantee-uri b 'URI=?)))

;;;; URI Authorities

(define-record-type <uri-authority>
    (%%make-uri-authority userinfo host port string)
    uri-authority?
  (userinfo uri-authority-userinfo)
  (host uri-authority-host)
  (port uri-authority-port)
  (string uri-authority-string))

(define uri-authority-camp (make-string-internment-camp))

(define (%make-uri-authority userinfo host port)
  (let ((string (uri-authority-components->string userinfo host port)))
    (intern uri-authority-camp string
      (lambda ()
        (%%make-uri-authority userinfo host port string)))))

(define (make-uri-authority userinfo host port)
  (%make-uri-authority
   (and userinfo (guarantee-uri-userinfo userinfo 'MAKE-URI-AUTHORITY))
   (guarantee-uri-host host 'MAKE-URI-AUTHORITY)
   (and port (guarantee-uri-port port 'MAKE-URI-AUTHORITY))))

(define (uri-authority-hash authority)
  (string-hash
   (uri-authority-string
    (guarantee-uri-authority authority 'URI-AUTHORITY-HASH))))

(define (uri-authority=? a b)
  (eq? (guarantee-uri-authority a 'URI-AUTHORITY=?)
       (guarantee-uri-authority b 'URI-AUTHORITY=?)))

(define (uri->alist uri)
  (let ((uri (guarantee-uri uri 'URI->ALIST)))
    (define (optional name accessor)
      (cond ((accessor uri) => (lambda (value) `((,name ,value))))
            (else '())))
    `(,@(optional 'SCHEME %uri-scheme)
      ,@(cond ((%uri-authority uri) => uri-authority->alist)
              (else '()))
      (PATH ,(%uri-path uri))
      ,@(optional 'QUERY %uri-query)
      ,@(optional 'FRAGMENT %uri-fragment))))

(define (uri-authority->alist authority)
  (let ((authority (guarantee-uri-authority authority 'URI-AUTHORITY->ALIST)))
    (define (optional name accessor)
      (cond ((accessor authority) => (lambda (value) `((,name ,value))))
            (else '())))
    `(,@(optional 'USERINFO uri-authority-userinfo)
      (HOST ,(uri-authority-host authority))
      ,@(optional 'PORT uri-authority-port))))

;;;; Predicates

(define (uri-scheme? object)
  (and (symbol? object)
       (match-string? uri-matcher:scheme (symbol->string object))))

(define (uri-userinfo? object)
  (and (string? object)
       (match-string? uri-matcher:userinfo object)))

(define (uri-host? object)
  (and (string? object)
       (match-string? uri-matcher:host object)))

(define (uri-port? object)
  (and (integer? object)
       (exact? object)
       (>= object 0)))

(define (uri-path? object)
  (let loop ((object object))
    (if (pair? object)
        (and (string? (car object))
             (loop (cdr object)))
        (null? object))))

(define (uri-path-absolute? path)
  (path-absolute? (guarantee-uri-path path 'URI-PATH-ABSOLUTE?)))

(define (path-absolute? path)
  (and (pair? path)
       (string=? "" (car path))))

(define (uri-path-relative? path)
  (path-relative? (guarantee-uri-path path 'URI-PATH-RELATIVE?)))

(define (path-relative? path)
  (not (path-absolute? path)))

(define (uri-query? object)
  (and (string? object)
       (match-string? uri-matcher:query object)))

(define (uri-fragment? object)
  (and (string? object)
       (match-string? uri-matcher:fragment object)))

(define (absolute-uri? object)
  (and (uri? object)
       (uri-absolute? object)))

(define (relative-uri? object)
  (and (uri? object)
       (uri-relative? object)))

(define (uri-absolute? object)
  (if (%uri-scheme object) #t #f))

(define (uri-relative? object)
  (if (%uri-scheme object) #f #t))

;;;; Guarantors

(define-syntax define-guarantor
  (syntax-rules ()
    ((define-guarantor guarantor predicate name)
     (define (guarantor obj . ctx)
       (if (predicate obj)
           obj
           (apply error (string-append "invalid uri " name ":") obj ctx))))))

(define-guarantor guarantee-uri-authority uri-authority? "authority")
(define-guarantor guarantee-uri-userinfo uri-userinfo? "userinfo")
(define-guarantor guarantee-uri-host uri-host? "host")
(define-guarantor guarantee-uri-port uri-port? "port")
(define-guarantor guarantee-uri-path uri-path? "path")
(define-guarantor guarantee-uri-query uri-query? "query")
(define-guarantor guarantee-uri-fragment uri-fragment? "fragment")

(define (guarantee-uri-scheme object . context)
  (define (lose) (apply error "Invalid URI scheme:" object context))
  (cond ((symbol? object)               ;++ Downcase here?
         (if (match-string? uri-matcher:scheme (symbol->string object))
             object
             (lose)))
        ((string? object)
         (if (match-string? uri-matcher:scheme object)
             (string->symbol (string-downcase object))
             (lose)))
        (else (lose))))

(define (guarantee-uri-absolute-path object . context)
  (if (uri-path? object)
      (if (path-absolute? object)
          object
          (apply error "Non-absolute URI path:" object context))
      (apply error "Invalid absolute URI path:" object context)))

(define (guarantee-uri-relative-path object . context)
  (if (uri-path? object)
      (if (path-absolute? object)
          object
          (apply error "Non-relative URI path:" object context))
      (apply error "Invalid relative URI path:" object context)))

(define (guarantee-uri object . context)
  (cond ((uri? object) object)
        ((maybe-string->uri object))
        (else (apply error "Invalid URI:" object context))))

(define (guarantee-absolute-uri object . context)
  (cond ((uri? object)
         (if (uri-absolute? object)
             object
             (apply error "Non-absolute URI:" object context)))
        ((maybe-string->absolute-uri object))
        (else (apply error "Invalid absolute URI:" object context))))

(define (guarantee-relative-uri object . context)
  (cond ((uri? object)
         (if (uri-relative? object)
             object
             (apply error "Non-relative URI:" object context)))
        ((maybe-string->relative-uri object))
        (else (apply error "Invalid relative URI:" object context))))

;;;; Merging URIs

(define (merge-uris uri base-uri . strict?-option)
  (let ((uri (guarantee-uri uri 'MERGE-URIS))
        (base-uri (guarantee-absolute-uri base-uri 'MERGE-URIS)))
    (define (make scheme authority path query)
      (make-uri scheme authority path query (uri-fragment uri)))
    (cond ((and (uri-scheme uri)
                (if (if (pair? strict?-option) (car strict?-option) #f)
                    #t
                    (not (equal? (uri-scheme uri) (uri-scheme base-uri)))))
           (make (uri-scheme uri)
                 (uri-authority uri)
                 (remove-dot-segments (uri-path uri))
                 (uri-query uri)))
          ((uri-authority uri)
           (make (uri-scheme base-uri)
                 (uri-authority uri)
                 (remove-dot-segments (uri-path uri))
                 (uri-query uri)))
          ((not (pair? (uri-path uri)))
           (make (uri-scheme base-uri)
                 (uri-authority base-uri)
                 (uri-path base-uri)
                 (or (uri-query uri)
                     (uri-query base-uri))))
          (else
           (make (uri-scheme base-uri)
                 (uri-authority base-uri)
                 (remove-dot-segments (merge-paths (uri-path uri) base-uri))
                 (uri-query uri))))))

(define (merge-paths path base-uri)
  (cond ((and (pair? path)              ;Absolute path
              (string=? "" (car path)))
         path)
        ((not (pair? (uri-path base-uri)))
         (if (uri-authority base-uri)
             (cons "" path)
             path))
        (else
         (let recur ((base-path (uri-path base-uri)))
           (if (pair? (cdr base-path))
               (cons (car base-path)
                     (recur (cdr base-path)))
               path)))))

;;;;; Removing `Dot' Segments

;;; This code looks very, very different from what the RFC explains,
;;; but the RFC's explanation is terrible and in terms of string
;;; buffers.  I believe that this code is equivalent, however.

(define (remove-dot-segments path)
  (let skip ((path path))
    (if (not (pair? path))
        '()
        (let ((segment (car path)))
          (cond ((or (string=? "." segment)
                     (string=? ".." segment))
                 (skip (cdr path)))
                ((string=? "" segment)
                 (cons "" (%remove-dot-segments (cdr path))))
                (else
                 (%remove-dot-segments path)))))))

(define (%remove-dot-segments path)
  (let loop ((input-path path) (output-path '()))
    (if (not (pair? input-path))
        (reverse output-path)
        (loop (cdr input-path)
              (let ((segment (car input-path)))
                (cond ((string=? "." segment)
                       (if (null? (cdr input-path))
                           (cons "" output-path)
                           output-path))
                      ((string=? ".." segment)
                       ((lambda (output-path)
                          (if (null? (cdr input-path))
                              (cons "" output-path)
                              output-path))
                        (if (pair? output-path)
                            (cdr output-path)
                            '())))
                      (else
                       (cons segment output-path))))))))

;;;; String<->URI Conversion

(define (uri->string uri)
  (%uri-string (guarantee-uri uri 'URI->STRING)))

(define (uri-components->string scheme authority path query fragment)
  (call-with-string-output-port
    (lambda (port)
      (write-uri-components scheme authority path query fragment port))))

(define (uri-authority->string authority)
  (uri-authority-string
   (guarantee-uri-authority authority 'URI-AUTHORITY->STRING)))

(define (uri-authority-components->string userinfo host port)
  (call-with-string-output-port
    (lambda (output-port)
      (write-authority-components userinfo host port output-port))))

(define (object->uri object)
  (guarantee-uri object 'OBJECT->URI))

(define (object->absolute-uri object)
  (guarantee-absolute-uri object 'OBJECT->ABSOLUTE-URI))

(define (object->relative-uri object)
  (guarantee-relative-uri object 'OBJECT->RELATIVE-URI))

(define (maybe-string->uri string)
  (%maybe-string->uri uri-parser:uri-reference string))

(define (maybe-string->absolute-uri string)
  (%maybe-string->uri uri-parser:uri string))

(define (maybe-string->relative-uri string)
  (%maybe-string->uri uri-parser:relative-ref string))

(define (string->uri string)
  (%string->uri uri-parser:uri-reference string 'STRING->URI))

(define (string->absolute-uri string)
  (%string->uri uri-parser:uri string 'STRING->ABSOLUTE-URI))

(define (string->relative-uri string)
  (%string->uri uri-parser:relative-ref string 'STRING->RELATIVE-URI))

(define (%maybe-string->uri parser string)
  (and (string? string)
       (or (soft-intern uri-camp string)
           (parse-string (parser:complete parser)
                         string
                         #f             ;No context
                         (lambda (uri context stream)
                           context stream ;ignore
                           uri)
                         (lambda (perror context stream)
                           context stream ;ignore
                           #f)))))

(define (%string->uri parser string caller)
  (if (not (string? string))
      (error "Invalid URI string:" string caller)
      (or (soft-intern uri-camp string)
          (parse-string (parser:complete parser)
                        string
                        #f              ;No context
                        (lambda (uri context stream)
                          context stream ;ignore
                          uri)
                        (lambda (perror context stream)
                          context stream ;ignore
                          (apply error "Malformed URI:" string caller
                                 `(at position ,(parse-error/position perror))
                                 ;; This is a little silly.
                                 (parse-error/messages perror)))))))

;;;; URI Writer

(define (write-uri uri port)
  (write-string (%uri-string (guarantee-uri uri 'WRITE-URI)) port))

(define (write-uri-components scheme authority path query fragment output-port)
  (if scheme (write-scheme scheme output-port))
  (if authority (write-uri-authority authority output-port))
  (if (pair? path)
      (if scheme
          (write-path path output-port)
          (begin
            (write-pct-encoded (car path) uri-char-set:pchar-nc output-port)
            (if (pair? (cdr path))
                (begin (write-string "/" output-port)
                       (write-path (cdr path) output-port))))))
  (if query (write-query query output-port))
  (if fragment (write-fragment fragment output-port)))

(define (write-scheme scheme output-port)
  (write-string (symbol->string scheme) output-port)
  (write-string ":" output-port))

(define (write-uri-authority authority output-port)
  (write-string (uri-authority-string authority) output-port))

(define (write-authority-components userinfo host port output-port)
  (write-string "//" output-port)
  (if userinfo
      (begin (write-pct-encoded userinfo uri-char-set:userinfo output-port)
             (write-string "@" output-port)))
  (write-string host output-port)
  (if port
      (begin (write-string ":" output-port)
             (write-string (number->string port #d10) output-port))))

(define (write-path path output-port)
  (let loop ((segments path))
    (write-pct-encoded (car segments) uri-char-set:pchar output-port)
    (if (pair? (cdr segments))
        (begin (write-string "/" output-port)
               (loop (cdr segments))))))

(define (write-query query output-port)
  (write-string "?" output-port)
  (write-string query output-port))

(define (write-fragment fragment output-port)
  (write-string "#" output-port)
  (write-pct-encoded fragment uri-char-set:fragment output-port))

(define (write-pct-encoded string char-set output-port)
  (do ((index 0 (+ index 1)))
      ((= index (string-length string)))
    (let ((char (string-ref string index)))
      (if (char-set-contains? char-set char)
          (write-char char output-port)
          (let ((code (char->ascii char)))
            (write-char #\% output-port)
            (write-char (hex-digit->char (quotient code #x10)) output-port)
            (write-char (hex-digit->char (remainder code #x10))
                        output-port))))))

;;;; URI Parser

;;; I have slightly modified the structure of the grammar for the sake
;;; of clarity, even though it deviates in minor ways from the RFC:
;;;
;;; - Instead of always using `[ "?" query ]' and `[ "#" fragment ]', I
;;;   have put the optional bracket and the leading character in the
;;;   production rules for query and fragment.
;;;
;;; - I have given the name AUTHORITY+PATH to the pattern `"//"
;;;   authority path-abempty", which is used both in the HIER-PART and
;;;   RELATIVE-PART production rules.
;;;
;;; I believe that that is all that I changed, but I could be wrong; I
;;; may have made some changes before I thought to document them.

(define-parser uri-parser:uri-reference
  (parser:deep-choice uri-parser:uri
                      uri-parser:relative-ref))

(define-parser uri-parser:uri
  (*parser
      (scheme uri-parser:scheme)
      ((parser:char= #\:))
      (authority.path uri-parser:hier-part)
      (query uri-parser:query)
      (fragment uri-parser:fragment)
    (parser:return
     (make-uri scheme
               (car authority.path)
               (cdr authority.path)
               query
               fragment))))

(define-parser uri-parser:relative-ref
  (*parser
      (authority.path uri-parser:relative-part)
      (query uri-parser:query)
      (fragment uri-parser:fragment)
    (parser:return
     (make-uri #f
               (car authority.path)
               (cdr authority.path)
               query
               fragment))))

(define-parser uri-parser:absolute-uri
  (*parser
      (scheme uri-parser:scheme)
      ((parser:char= #\:))
      (authority.path uri-parser:hier-part)
      (query uri-parser:query)
    (parser:return
     (make-uri scheme
               (car authority.path)
               (cdr authority.path)
               query
               #f))))

(define-parser uri-parser:scheme
  (*parser (string (parser:match->string uri-matcher:scheme))
    (begin (string-downcase! string)    ;++ ARGH!
           (parser:return (string->symbol string)))))

;;; The following two differ by PATH-ROOTLESS versus PATH-NOSCHEME.

(define-parser uri-parser:hier-part
  (parser:choice uri-parser:authority+path
                 (*parser (path uri-parser:path)
                   (parser:return (cons #f path)))))

(define-parser uri-parser:relative-part
  (parser:choice uri-parser:authority+path
                 (*parser (path
                           (parser:choice uri-parser:path-absolute
                                          uri-parser:path-noscheme
                                          uri-parser:path-empty))
                   (parser:return (cons #f path)))))

(define-parser uri-parser:authority+path
  (parser:backtrackable                 ; We may need to back out of a slash,
   (*parser                             ; for absolute paths without authority.
       ((parser:string= "//"))
       (authority uri-parser:authority)
       (path uri-parser:path-abempty)
     (parser:return (cons authority path)))))

(define-parser uri-parser:query
  (*parser (context (parser:context))
   (uri-parser:optional-appendage #\? uri-char-set:query #f)))

(define-parser uri-parser:fragment
  (uri-parser:optional-appendage #\# uri-char-set:fragment #t))

(define-parser (uri-parser:optional-appendage delimiter char-set decode?)
  (parser:optional #f
    (parser:sequence (parser:char= delimiter)
                     (if decode?
                         (uri-parser:pct-encoded-string char-set)
                         (uri-parser:pct-encoded-string-raw char-set)))))

(define-parser (uri-parser:pct-encoded-string-nz char-set)
  (parser:string:at-least 1
    (parser:choice (parser:char-in-set char-set)
                   uri-parser:pct-encoded)))

(define-parser (uri-parser:pct-encoded-string char-set)
  (parser:string:repeated
   (parser:choice (parser:char-in-set char-set)
                  uri-parser:pct-encoded)))

(define-parser uri-parser:pct-encoded
  (*parser
      ((parser:char= #\%))
      (a uri-parser:hex-digit)
      (b uri-parser:hex-digit)
    (parser:return (ascii->char (+ (* a #x10) b)))))

(define-parser (uri-parser:pct-encoded-string-raw char-set)
  (parser:string:repeated
   (parser:choice (parser:char-in-set char-set)
                  uri-parser:pct-encoded-raw)))

(define-parser uri-parser:pct-encoded-raw
  (*parser
      ((parser:char= #\%))
      (a uri-parser:hex-char)
      (b uri-parser:hex-char)
    (parser:return (string #\% a b))))


(define-parser uri-parser:authority
  (*parser
      (userinfo
       (parser:choice (parser:backtrackable
                       (*parser (userinfo uri-parser:userinfo)
                                ((parser:char= #\@))
                         (parser:return userinfo)))
                      (parser:return #f)))
      (host uri-parser:host)
      (port
       (parser:optional #f
         (parser:sequence (parser:char= #\:) uri-parser:port)))
    (parser:return
     (make-uri-authority userinfo host port))))

(define-parser uri-parser:userinfo
  (uri-parser:pct-encoded-string uri-char-set:userinfo))

(define-parser uri-parser:host
  (*parser (string (parser:match->string uri-matcher:host))
    (begin (string-downcase! string) (parser:return string))))

(define-parser uri-parser:port
  (*parser (string (parser:match->string uri-matcher:port))
    (parser:return (and (positive? (string-length string))
                        (string->number string #d10)))))

(define (string->uri-path string)
  (parse-string (parser:complete uri-parser:path)
                string
                #f ;No context
                (lambda (path context stream)
                  context stream ;ignore
                  path)
                (lambda (perror context stream)
                  context stream ;ignore
                  (apply error "Malformed URI path:" string 'string->uri
                         `(at position ,(parse-error/position perror))
                         (parse-error/messages perror)))))

(define (maybe-string->uri-path string)
  (and
    string
    (parse-string (parser:complete uri-parser:path)
                  string
                  #f                    ;No context
                  (lambda (path context stream)
                    context stream      ;ignore
                    path)
                  (lambda (perror context stream)
                    context stream      ;ignore
                    #f))))

(define-parser uri-parser:path
  (parser:choice uri-parser:path-absolute
                 uri-parser:path-rootless
                 uri-parser:path-empty))

(define-parser uri-parser:path-abempty
  (*parser (segments uri-parser:segments)
    (parser:return (cons "" segments))))

(define-parser uri-parser:path-absolute
  (parser:sequence
   (parser:char= #\/)
   (parser:optional '("" "")
     (*parser
         (initial-segment uri-parser:segment-nz)
         (trailing-segments uri-parser:segments)
       (parser:return (cons "" (cons initial-segment trailing-segments)))))))

(define-parser uri-parser:path-rootless
  (*parser (initial-segment uri-parser:segment-nz)
           (trailing-segments uri-parser:segments)
    (parser:return (cons initial-segment trailing-segments))))

(define-parser uri-parser:path-noscheme
  (*parser (initial-segment uri-parser:segment-nz-nc)
           (trailing-segments uri-parser:segments)
    (parser:return (cons initial-segment trailing-segments))))

(define-parser uri-parser:path-empty (parser:return '()))

(define-parser uri-parser:segments
  (parser:list:repeated
   (parser:sequence (parser:char= #\/) uri-parser:segment)))

(define-parser uri-parser:segment
  (uri-parser:pct-encoded-string uri-char-set:pchar))

(define-parser uri-parser:segment-nz
  (uri-parser:pct-encoded-string-nz uri-char-set:pchar))

(define-parser uri-parser:segment-nz-nc
  (uri-parser:pct-encoded-string-nz uri-char-set:pchar-nc))

;;;; URI Matchers

(define-matcher uri-matcher:uri-reference
  (matcher:deep-choice uri-matcher:uri
                       uri-matcher:relative-ref))

(define-matcher uri-matcher:uri
  (matcher:sequence uri-matcher:scheme
                    (matcher:char= #\:)
                    uri-matcher:hier-part
                    uri-matcher:query
                    uri-matcher:fragment))

(define-matcher uri-matcher:relative-ref
  (matcher:sequence uri-matcher:relative-part
                    uri-matcher:query
                    uri-matcher:fragment))

(define-matcher uri-matcher:absolute-uri
  (matcher:sequence uri-matcher:scheme
                    (matcher:char= #\:)
                    uri-matcher:hier-part
                    uri-matcher:query))

(define-matcher uri-matcher:scheme
  (matcher:sequence
   (matcher:char-in-set char-set:letter)
   (matcher:repeated
    (matcher:char-in-set
     (char-set-adjoin char-set:letter+digit #\+ #\- #\.)))))

(define-matcher uri-matcher:hier-part
  (matcher:choice uri-matcher:authority+path
                  uri-matcher:path-absolute
                  uri-matcher:path-rootless
                  uri-matcher:path-empty))

(define-matcher uri-matcher:relative-part
  (matcher:choice uri-matcher:authority+path
                  uri-matcher:path-absolute
                  uri-matcher:path-noscheme
                  uri-matcher:path-empty))

(define-matcher uri-matcher:authority+path
  (matcher:sequence (matcher:char= #\/)
                    (matcher:char= #\/)
                    uri-matcher:authority
                    uri-matcher:path-abempty))

(define-matcher uri-matcher:query
  (uri-matcher:optional-appendage #\? uri-char-set:query))

(define-matcher uri-matcher:fragment
  (uri-matcher:optional-appendage #\# uri-char-set:fragment))

(define-matcher (uri-matcher:optional-appendage delimiter char-set)
  (matcher:optional
   (matcher:sequence (matcher:char= delimiter)
                     (uri-matcher:pct-encoded-string char-set))))

;;;;; Authority and Path

(define-matcher uri-matcher:authority
  (matcher:sequence (matcher:optional
                     (matcher:sequence uri-matcher:userinfo
                                       (matcher:char= #\@)))
                    uri-matcher:host
                    (matcher:optional
                     (matcher:sequence (matcher:char= #\:)
                                       uri-matcher:port))))

(define-matcher uri-matcher:userinfo
  (uri-matcher:pct-encoded-string uri-char-set:userinfo))

(define-matcher uri-matcher:host
  (matcher:deep-choice uri-matcher:ip-literal
                       uri-matcher:ipv4-address
                       uri-matcher:reg-name))

(define-matcher uri-matcher:port
  (matcher:repeated (matcher:char-in-set char-set:digit)))

(define-matcher uri-matcher:path-abempty
  uri-matcher:segments)

(define-matcher uri-matcher:path-absolute
  (matcher:sequence (matcher:char= #\/)
                    (matcher:optional
                     (matcher:sequence uri-matcher:segment-nz
                                       uri-matcher:segments))))

(define-matcher uri-matcher:path-noscheme
  (matcher:sequence uri-matcher:segment-nz-nc uri-matcher:segments))

(define-matcher uri-matcher:path-rootless
  (matcher:sequence uri-matcher:segment-nz uri-matcher:segments))

(define-matcher uri-matcher:path-empty
  (matcher:epsilon))

(define-matcher uri-matcher:segments
  (matcher:repeated
   (matcher:sequence (matcher:char= #\/) uri-matcher:segment)))

(define-matcher uri-matcher:segment
  (uri-matcher:pct-encoded-string uri-char-set:pchar))

(define-matcher uri-matcher:segment-nz
  (uri-matcher:pct-encoded-string-nz uri-char-set:pchar))

(define-matcher uri-matcher:segment-nz-nc
  (uri-matcher:pct-encoded-string-nz uri-char-set:pchar-nc))

;;;;; Hosts and Percent-Encoded Strings

(define-matcher uri-matcher:ip-literal
  (matcher:sequence (matcher:char= #\[)
                    (matcher:choice uri-matcher:ipvfuture
                                    uri-matcher:ipv6-address)
                    (matcher:char= #\])))

(define-matcher uri-matcher:ipvfuture
  (matcher:sequence
   (matcher:char-ci= #\v)
   (matcher:at-least 1 (matcher:char-in-set char-set:hex-digit))
   (matcher:char= #\.)
   (matcher:at-least 1 (matcher:char-in-set uri-char-set:ipvfuture))))

(define-matcher uri-matcher:ipv4-address
  (matcher:sequence uri-matcher:dec-octet
                    (matcher:char= #\.)
                    uri-matcher:dec-octet
                    (matcher:char= #\.)
                    uri-matcher:dec-octet
                    (matcher:char= #\.)
                    uri-matcher:dec-octet))

(define-matcher uri-matcher:dec-octet   ;++ What a mess!
  (matcher:deep-choice
   (matcher:char-in-set char-set:digit)
   (matcher:sequence (matcher:char-in-set (string->char-set "123456789"))
                     (matcher:char-in-set char-set:digit))
   (matcher:sequence (matcher:char= #\1)
                     (matcher:char-in-set char-set:digit)
                     (matcher:char-in-set char-set:digit))
   (matcher:sequence (matcher:char= #\2)
                     (matcher:char-in-set (string->char-set "01234"))
                     (matcher:char-in-set char-set:digit))
   (matcher:sequence (matcher:char= #\2)
                     (matcher:char= #\5)
                     (matcher:char-in-set (string->char-set "012345")))))

(define-matcher uri-matcher:reg-name
  (matcher:repeated (matcher:char-in-set uri-char-set:reg-name)))

(define-matcher (uri-matcher:pct-encoded-string char-set)
  (matcher:repeated
   (matcher:choice uri-matcher:pct-encoded
                   (matcher:char-in-set char-set))))

(define-matcher (uri-matcher:pct-encoded-string-nz char-set)
  (matcher:at-least 1
    (matcher:choice uri-matcher:pct-encoded
                    (matcher:char-in-set char-set))))

(define-matcher uri-matcher:pct-encoded
  (matcher:sequence (matcher:char= #\%)
                    (matcher:char-in-set char-set:hex-digit)
                    (matcher:char-in-set char-set:hex-digit)))

;;;;;; The Hair of Matching IPv6 Addresses

;;; At least this is a nice example of how to easily make hairy parsers
;;; locally concise.

(define-matcher uri-matcher:ipv6-address
  (let ((seq matcher:sequence)
        (:: (matcher:sequence (matcher:char= #\:) (matcher:char= #\:)))
        (h16 uri-matcher:h16)
        (ls32 uri-matcher:ls32)
        (n-h16: uri-matcher:h16-exactly)
        (n*-h16: uri-matcher:h16-at-least)
        (? matcher:optional))
    (matcher:deep-choice
     (seq                               (n-h16: 6)      ls32)
     (seq                       ::      (n-h16: 5)      ls32)
     (seq (? (n*-h16: 0))       ::      (n-h16: 4)      ls32)
     (seq (? (n*-h16: 1))       ::      (n-h16: 3)      ls32)
     (seq (? (n*-h16: 2))       ::      (n-h16: 2)      ls32)
     (seq (? (n*-h16: 3))       ::      (n-h16: 1)      ls32)
     (seq (? (n*-h16: 4))       ::                      ls32)
     (seq (? (n*-h16: 5))       ::                      h16)
     (seq (? (n*-h16: 6))       ::))))

(define-matcher uri-matcher:h16
  (matcher:between 1 4 (matcher:char-in-set char-set:hex-digit)))

(define-matcher uri-matcher:ls32
  (matcher:deep-choice
   (matcher:sequence uri-matcher:h16
                     (matcher:char= #\:)
                     uri-matcher:h16)
   uri-matcher:ipv4-address))

(define-matcher (uri-matcher:h16-exactly number)
  (matcher:exactly number
    (matcher:sequence uri-matcher:h16
                      (matcher:char= #\:))))

(define-matcher (uri-matcher:h16-at-least number)
  (matcher:sequence
   (matcher:at-least number
     (matcher:sequence uri-matcher:h16
                       (matcher:char= #\:)))
   uri-matcher:h16))

;;;; URI-Related Character Sets

(define uri-char-set:gen-delims
  (string->char-set ":/?#[]@"))

(define uri-char-set:sub-delims
  (string->char-set "!$&'()*+,;="))

(define uri-char-set:unreserved
  (char-set-union char-set:letter+digit
                  (string->char-set "-._~")))

(define uri-char-set:reserved
  (char-set-union uri-char-set:gen-delims
                  uri-char-set:sub-delims))

(define uri-char-set:userinfo
  (char-set-union uri-char-set:unreserved
                  uri-char-set:sub-delims
                  (char-set #\:)))

(define uri-char-set:ipvfuture
  (char-set-union uri-char-set:unreserved
                  uri-char-set:sub-delims
                  (char-set #\:)))

(define uri-char-set:reg-name
  (char-set-union uri-char-set:unreserved
                  uri-char-set:sub-delims))

(define uri-char-set:pchar
  (char-set-union uri-char-set:unreserved
                  uri-char-set:sub-delims
                  (char-set #\: #\@)))

(define uri-char-set:pchar-nc
  (char-set-delete uri-char-set:pchar #\:))

(define uri-char-set:query
  (char-set-union uri-char-set:pchar
                  (char-set #\/ #\?)))

(define uri-char-set:fragment
  (char-set-union uri-char-set:pchar
                  (char-set #\/ #\?)))

;;;; Utilities

(define (char->hex-digit char)
  (let ((code (char->ascii char)))
    (cond ((char-numeric?    char) (- code (char->ascii #\0)))
          ((char-upper-case? char) (- (+ code #xA) (char->ascii #\A)))
          ((char-lower-case? char) (- (+ code #xA) (char->ascii #\a)))
          (else (error "Invalid hex digit character:" char 'CHAR->HEX-DIGIT)))))

(define (hex-digit->char value)
  (cond ((< value #xA) (ascii->char (+ value (char->ascii #\0))))
        ((< value #x10) (ascii->char (+ (- value #xA) (char->ascii #\A))))
        (else (error "Invalid hex digit value:" value 'HEX-DIGIT->CHAR))))

(define-parser uri-parser:hex-digit
  (*parser (char (parser:char-in-set char-set:hex-digit))
    (parser:return (char->hex-digit char))))

(define-parser uri-parser:hex-char
  (parser:char-in-set char-set:hex-digit))
