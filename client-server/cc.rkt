#lang racket/base

(require racket/tcp)
(require rnrs/io/ports-6)

(printf "Trying to connect to server... ")
(define-values (in out) (tcp-connect "localhost" 12346))
(printf "Ok.\n")

(define (handle-server-msg p)
  (let* ((msg (read p))
	 (q (equal? msg 'quit)))
    (when (not q)
      (displayln msg) (flush-output))
    ; Return port as sync result
    p))

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
