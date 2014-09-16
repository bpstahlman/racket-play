#lang racket
(provide handle-form-maybe)

; TODO: Handle eof...
(define (handle-form-maybe in msg-handler eof-handler )
  (let till-empty ([br (byte-ready? in)])
    (when br
      (let ([c (peek-char in 0)])
	(printf "peeked-byte = ~a\n" c) (flush-output)
	(if (eof-object? c)
	  (begin (printf "Calling eof-handler\n") (flush-output) (eof-handler)) 
	  (begin
	    (printf "About to peek-string!\n") (flush-output)
	    (if (char-whitespace? c)
	      (read-char in)
	      (msg-handler (read in)))  
	    (till-empty (byte-ready? in))))))))
