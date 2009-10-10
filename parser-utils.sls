(library (ocelotl parser-utils)
  (export parse-input-bytes-as-latin1)
  (import (rnrs)
          (spells lazy)
          (spells lazy-streams)
          (parscheme parser-combinators))

(define (parse-input-bytes-as-latin1 parser input-port context win lose)
  (parse-stream parser
                (let recur ()
                  (lazy (let ((byte (get-u8 input-port)))
                          (if (eof-object? byte)
                              stream-nil
                              (stream-cons (integer->char byte) (recur))))))
                (cons 1 1)
                (lambda (position char)
                  (let ((line (car position))
                        (column (cdr position)))
                    (if (char=? char #\newline)
                        (cons (+ line 1) 1)
                        (cons line (+ column 1)))))
                context
                win
                lose))

)
