#lang racket/base

(require racket/tcp)
(require rnrs/io/ports-6)
(require racket/string) ; string-trim

(printf "Trying to connect to server... ")
(define-values (in out) (tcp-connect "localhost" 12346))
(printf "Ok.\n")

(define (process-server-msg msg)
  ; TODO!!
  (printf "Client received: `~a'\n" msg)
  (flush-output))

; TODO: Fix this - like server!
; Eventually, move into common...
(define (handle-server-msg in)
  (printf "Inside handle-server-msg!\n") (flush-output)
  (define spc (peek-string 1 0 in))
  (printf "Just peeked string\n `~a'" spc) (flush-output)
  (if (string=? "" (string-trim spc))
    (begin (printf "About to read-string\n") (flush-output) (read-string 1 in) "")
    (begin (printf "About to read form\n") (flush-output)
	   (process-server-msg (read in)))))

(define (handle-rdy-to-send p)
  (when (byte-ready? p)
    (printf "Ready to send!\n") (flush-output)
    (write (read p) out)
    (printf "Just sent\n") (flush-output)
    ; Caveat: Display (don't write) whitespace to delimit the datum for server read.
    (display " " out)
    (flush-output out)))

(define server-msg-evt (wrap-evt in handle-server-msg))
(define rdy-to-send-evt (wrap-evt (standard-input-port) handle-rdy-to-send))

(let loop ()
  (sync rdy-to-send-evt server-msg-evt)
  (loop))


(printf "Closing connection.\n")
(close-input-port in)
(close-output-port out)
