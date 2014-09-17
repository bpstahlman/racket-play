#lang racket
(provide handle-form-maybe)

; Discard any leading whitespace on specified port, then read form if
; possible, applying msg-handler to the form read. If no form can be
; read, simply return. If eof is encountered, invoke the supplied
; eof-handler.
(define (handle-form-maybe in msg-handler eof-handler)
  (let till-empty ([br (byte-ready? in)])
    (when br
      (let ([c (peek-char in 0)])
	(if (eof-object? c)
	  (eof-handler) 
	  (begin
	    (if (char-whitespace? c)
	      (read-char in)
	      (msg-handler (read in)))  
	    (till-empty (byte-ready? in))))))))
