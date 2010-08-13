;;; http-client.sls --- Simple HTTP client library

;; Copyright (C) 2005,2009,2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Based on Public Domain code written by Taylor R. Campbell.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (ocelotl net http-client)
  (export
    default-http-uri-authority
    with-default-http-uri-authority
    
    earliest-http-version
    latest-http-version

    call-with-http-response
    http-get
    http-head
    http-post
    
    read-http-entity-body
    )
  (import
    (except (rnrs) define-record-type)
    (rnrs lists)
    (only (srfi :1) every)
    (srfi :8 receive)
    (srfi :9 records)
    (only (srfi :13) string-join string-trim-right)
    (srfi :14 char-sets)
    (srfi :39 parameters)
    (wak riastreams)
    (wak foof-loop)
    (wak foof-loop nested)
    (wak parscheme parse-errors)
    (wak parscheme parser-combinators)
    (wak parscheme text-parser-combinators)
    (spells network)
    (ocelotl parser-utils)
    (ocelotl net rfc822)
    (ocelotl net http)
    (ocelotl net uri)
    )



(define (http-head uri header-fields)
  (call-with-http-response 'HEAD uri header-fields #f
    (lambda (http-response body-input-port)
      body-input-port                   ;ignore
      http-response)))

(define (http-get uri header-fields)
  (call-with-http-response 'GET uri header-fields #f
    (lambda (http-response body-input-port)
      (values http-response
              (read-http-entity-body http-response body-input-port)))))

(define (http-post uri header-fields body)
  (call-with-http-response 'POST uri header-fields body
    (lambda (http-response body-input-port)
      (values http-response
              (read-http-entity-body http-response body-input-port)))))

(define (read-http-entity-body http-response input-port)
  (or (let ((header-fields (http-response/header-fields http-response)))
        (cond ((first-http-header-field-value header-fields 'connection #f)
               => (lambda (value)
                    (and (string-ci=? "close" value)
                         (get-bytevector-all input-port))))
              ((first-http-header-field-value header-fields 'content-length #f)
               => (lambda (value)
                    (cond ((string->number value #d10)
                           => (lambda (content-length)
                                (get-bytevector-n input-port content-length)))
                          (else #f))))
              (else ;read till end of connection
               (get-bytevector-all input-port))))
      (begin
        (warn 'read-http-entity-body
              "Unable to determine entity length of response:" http-response)
        #f)))


;;; I wonder whether this interface ought to separate the authority and
;;; the request URI, since they really are distinct, and requests to
;;; proxies must send absolute URIs.

(define (call-with-http-response method uri header-fields body receiver)
  (receive (uri authority) (decompose-http-request-uri uri)
    (call-with-http-connection authority
      (lambda (connection)
        (send-http-request connection method uri header-fields body)
        (close-port (http-connection/output-port connection))
        (receive results (receiver (receive-http-response connection)
                                   (http-connection/input-port connection))
          (close-connection (http-connection/connection connection))
          (apply values results))))))

(define $default-http-uri-authority (make-parameter #f))
(define (default-http-uri-authority) ($default-http-uri-authority))
(define (with-default-http-uri-authority authority thunk)
  (parameterize (($default-http-uri-authority authority))
    (thunk)))

(define (decompose-http-request-uri uri)
  (cond ((or (uri-authority uri)
             (default-http-uri-authority))
         => (lambda (authority)
              (values (make-uri #f      ;No scheme,
                                #f      ; no authority -- relative.
                                (uri-path uri)
                                (uri-query uri)
                                (uri-fragment uri))
                      (make-uri-authority (uri-authority-userinfo authority)
                                          (uri-authority-host authority)
                                          (or (uri-authority-port authority)
                                              80)))))
        (else
         (assertion-violation 'decompose-http-request-uri
                              "Unable to determine authority for HTTP request URI:"
                              uri))))


;;;; HTTP Connections

;++ Implement a concurrent connection pool.

(define-record-type <http-connection>
    (make-http-connection uri-authority connection)
    http-connection?
  (uri-authority http-connection/uri-authority)
  (connection http-connection/connection))

(define (http-connection/input-port http-connection)
  (connection-input-port (http-connection/connection http-connection)))

(define (http-connection/output-port http-connection)
  (connection-output-port (http-connection/connection http-connection)))

(define (call-with-http-connection authority receiver)
  (let ((connection (open-http-connection authority)))
    (receive results (receiver connection)
      (close-http-connection connection)
      (apply values results))))

(define (open-http-connection authority)
  (make-http-connection authority
                        (open-tcp-connection (uri-authority-host authority)
                                             (uri-authority-port authority))))

(define (http-connection-open? connection)
  connection                            ;ignore
  #t)

(define (close-http-connection connection)
  (close-connection (http-connection/connection connection)))

(define (uri-authority/host-string authority)
  (string-append (uri-authority-host authority)
                 (let ((port (uri-authority-port authority)))
                   (if (and port (not (= port 80)))
                       (string-append ":" (number->string port #d10))
                       ""))))

(define (http-connection/host-string connection)
  (uri-authority/host-string (http-connection/uri-authority connection)))

(define (send-http-request connection method uri header-fields body)
  (receive (body content-length)
           (cond ((string? body)
                  (let ((bytes (string->utf8 body)))
                    (values bytes (bytevector-length bytes))))
                 ((bytevector? body)
                  (values body (bytevector-length body)))
                 ((not body)
                  (values #f 0))
                 (else
                  (values #f #f)))
    (write-http-request method
                        uri
                        (adjoin-http-header-fields
                         (list (make-http-header-field
                                'host
                                (http-connection/host-string connection)))
                         header-fields
                         (if content-length
                             (list (make-http-header-field
                                    'content-length
                                    (number->string content-length #d10)))
                             '()))
                        body
                        (http-connection/output-port connection))))

(define (receive-http-response connection)
  (read-http-response (http-connection/input-port connection)))


;;; Supported HTTP versions

(define earliest-http-version (make-http-version 1 0))
(define latest-http-version (make-http-version 1 0))


;;;; HTTP Requests

(define (write-http-request method uri header-fields body port)
  (write-http-request-line method uri port)
  (write-http-header-fields header-fields port)
  (when body
    (write-http-body body port))
  (flush-output-port port))

(define (write-http-request-line method uri port)
  (put-utf8-crlf port
                 (http-method->string method) " "
                 (http-request-uri->string uri) " "
                 (http-version->string latest-http-version)))

(define (http-method->string method)
  (cond ((symbol? method) (string-upcase (symbol->string method)))
        ((string? method) method)
        (else
         (assertion-violation 'http-method->string
                              "Invalid HTTP request method:" method))))

(define (http-request-uri->string uri)
  (cond ((eq? '*  uri) "*")
        ((uri?    uri) (uri->string uri))
        ((string? uri) string)
        ((and (pair? uri)
              (list? uri)
              (every string? uri))
         (string-join uri "/"))
        (else
         (assertion-violation 'http-request-uri->string
                              "Invalid HTTP request URI:" uri))))

(define (http-version->string v)
  (string-append "HTTP/"
                 (number->string (car v))
                 "."
                 (number->string (cdr v))))

(define (write-http-header-fields header-fields port)
  (for-each (lambda (header-field)
              (put-utf8-crlf port
                             (field-name->string
                              (http-header-field/name header-field))
                             ": "
                             (field-value->string
                              (http-header-field/value header-field))))
            header-fields)
  (put-utf8-crlf port))

(define (field-name->string name)
  (symbol->string name)) ;; Prettify?

(define (field-value->string value)
  (cond ((string? value) value)
        ((number? value) (number->string value))
        (else
         (assertion-violation
          'field-value->string
          "don't know how to stringify HTTP headers field" value))))

(define (write-http-body body port)
  (cond ((bytevector? body)
         (put-bytevector port body))
        ((procedure? body)
         (body port))
        (else
         (assertion-violation
          'write-http-request-body
          "don't know how to send HTTP request body" body))))

(define (read-http-response input-port)
  (parse-input-bytes-as-latin1
   http-parser:response
   input-port
   #f                 ;No context
   (lambda (response context stream)
     context stream   ;ignore
     response)
   (lambda (perror context stream)
     (apply error
            "Malformed HTTP response:"
            (parse-error/position perror)
            (parse-error/messages perror)))))


;;;;; Response Parser

(define-parser http-parser:response
  (*parser
      ((parser:string= "HTTP/"))
      (version http-parser:version)
      (http-parser:lws)
      (status-code http-parser:status-code)
      (http-parser:lws)
      (reason http-parser:reason-phrase)
      (http-parser:crlf)
      ;; This eats the final CRLF.
      (header-fields http-parser:header-fields)
    (parser:return
     (make-http-response version
                         status-code
                         reason
                         header-fields))))

(define-parser http-parser:version
  (*parser
      (major http-parser:version-part)
      ((parser:char= #\.))
      (minor http-parser:version-part)
    (let ((version (make-http-version major minor)))
      (if (or (http-version<=? version earliest-http-version)
              (http-version<=? latest-http-version version))
          (parser:return version)
          (parser:error "Unsupported HTTP version:" version)))))

(define-parser http-parser:version-part
  (parser:decimal-string->number
   (parser:string:at-least 1 (parser:char-in-set char-set:digit))
   (lambda (string)
     (parser:error "Non-numeric version part:" string))))

(define-parser http-parser:status-code
  (parser:decimal-string->number
   (parser:string:exactly 3 (parser:char-in-set char-set:digit))
   (lambda (string)
     (parser:error "Non-numeric status code:" string))))

(define-parser (parser:decimal-string->number string-parser error-parser)
  (*parser (string string-parser)
    (cond ((string->number string #d10)
           => parser:return)
          (else (error-parser string)))))

(define-parser http-parser:reason-phrase
  (parser:string:repeated (parser:char-not-in-set http-char-set:crlf)))


;;;;;; Parsing Header Fields

(define-parser http-parser:header-fields
  (parser:list:repeated-until http-parser:crlf http-parser:header-field))

(define-parser http-parser:header-field
  (*parser
      (name http-parser:header-field-name)
      (value http-parser:header-field-value)
    (parser:return
     (make-http-header-field name value))))

(define-parser http-parser:header-field-name
  (parser:map intern
    (parser:string:at-least-until 1 (parser:char= #\:)
      (parser:char-in-set http-char-set:header-field-name))))

(define-parser http-parser:header-field-value
  (*parser
      (initial-line http-parser:header-field-initial-line)
      (continuation-lines
       (parser:list:repeated http-parser:header-field-continuation-line))
    (parser:return
     ;++ I wonder whether it would be better to have header fields map
     ;++ names to *lists* of values, for each separate line.  I don't
     ;++ think that the intent of the HTTP RFC is for implementations
     ;++ to distinguish this, but it would not surprise me if certain
     ;++ protocols did.
     (string-join/infix " " (cons initial-line continuation-lines)))))

(define-parser http-parser:header-field-initial-line
  (parser:sequence http-parser:lws* http-parser:header-field-line))

(define-parser http-parser:header-field-continuation-line
  (parser:sequence http-parser:lws+ http-parser:header-field-line))

(define-parser http-parser:header-field-line
  (parser:map string-trim-right
    (parser:string:repeated-until http-parser:crlf
      (parser:char-in-set http-char-set:header-field-value))))


;;;;;; Parsing Utilities and Character Sets

(define http-char-set:lws               ;Linear White Space
  (char-set (integer->char #x09)          ;  TAB
            (integer->char #x20)))        ;  SPC

(define-parser http-parser:lws
  (parser:char-in-set http-char-set:lws))

(define-parser http-parser:lws*
  (parser:noise:repeated http-parser:lws))

(define-parser http-parser:lws+
  (parser:noise:at-least 1 http-parser:lws))

(define http-char-set:crlf
  (char-set (integer->char #x0D)
            (integer->char #x0A)))

(define-parser http-parser:crlf
  (let ((carriage-return (parser:char= (integer->char #x0D)))
        (line-feed (parser:char= (integer->char #x0A))))
    (parser:choice
     (parser:sequence carriage-return
                      (parser:optional-noise line-feed))
     line-feed)))

(define http-char-set:separator
  (char-set #\( #\) #\< #\> #\@
            #\, #\; #\: #\\ #\"
            #\/ #\[ #\] #\? #\=
            #\{ #\} #\space
            ;; Horizontal tab
            (integer->char #x09)))

(define http-char-set:token
  (char-set-complement
   (char-set-union char-set:iso-control
                   http-char-set:separator)))

(define http-char-set:header-field-name
  ;; http-char-set:token
  (char-set-complement (char-set-adjoin http-char-set:crlf #\:)))

(define http-char-set:header-field-value
  (char-set-union (char-set-complement char-set:iso-control)
                  http-char-set:lws))

;;;; Header Fields

(define (first-http-header-field header-fields name)
  (assoc name header-fields))

(define (first-http-header-field-value header-fields name default)
  (cond ((first-http-header-field header-fields name) => cdr)
        (else default)))

(define (find-http-header-field-value header-fields name)
  (string-join/infix "," (all-http-header-field-values name header-fields)))

(define (all-http-header-fields header-fields name)
  (collect-list (for header-field (in-list header-fields))
      (if (eq? name (http-header-field/name header-field)))
    header-field))

(define (all-http-header-field-values header-fields name)
  (collect-list (for header-field (in-list header-fields))
      (if (eq? name (http-header-field/name header-field)))
    (http-header-field/value header-field)))

(define (adjoin-http-header-fields left header-fields right)
  (define (clean other-header-fields initial-tail)
    (collect-list (initial initial-tail)
        (for header-field (in-list other-header-fields))
        (if (not (first-http-header-field
                  header-fields
                  (http-header-field/name header-field))))
      header-field))
  (clean left (append header-fields (clean right '()))))

(define (parse-http-header-field-value parser header-fields name context
                                       win lose)
  (parse-stream parser
                (stream-http-header-field-value header-fields name)
                #f                      ;No position
                (lambda (position token) token position)
                context
                win
                lose))

(define (stream-http-header-field-value header-fields name)
  (let ((comma (stream-cons #\, stream-nil)))
    (lazy-loop recur ((for header-field (in-list header-fields))
                      (with prefix stream-nil))
      => stream-nil
      (if (eq? name (http-header-field/name header-field))
          (stream-append prefix
                         (string->stream
                          (http-header-field/value header-field))
                         (recur (=> prefix comma)))
          (recur)))))


;;; Utilities

(define (warn who message . irritants)
  (raise-continuable
    (condition (make-warning)
               (make-who-condition who)
               (make-message-condition message)
               (make-irritants-condition irritants))))


(define (put-utf8-crlf port . args)
  (let loop ((args args))
    (if (null? args)
        (put-bytevector port '#vu8(13 10))
        (let ((obj (car args)))
          (cond ((string? obj)
                 (put-bytevector port (string->utf8 obj)))
                (else
                 (put-bytevector port obj)))
          (loop (cdr args))))))

(define (intern string)
  (string->symbol (string-downcase string)))

(define (string-join/infix separator strings)
  (string-join strings separator 'infix))

)
