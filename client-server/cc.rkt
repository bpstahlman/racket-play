#lang racket/base

(require racket/tcp)

(printf "Trying to connect to server... ")
(define-values (in out) (tcp-connect "localhost" 12346))
(printf "Ok.\n")

(define (handle-server-msg sr)
  (let* ((msg (read sr))
	 (q (equal? msg 'quit)))
    (when (not q)
      (displayln msg) (flush-output))
    #t))

(define (handle-rdy-to-send sr)
  (write (read sr) out)
  ; Caveat: Display (don't write) whitespace to delimit the datum for server read.
  (display " " out)
  (flush-output out)
  #f)

(define server-msg-evt (wrap-evt in handle-server-msg))
(define rdy-to-send-evt (wrap-evt (current-input-port) handle-rdy-to-send))

(let loop ()
  (let ((sr (sync server-msg-evt rdy-to-send-evt)))
    (loop)))


(printf "Closing connection.\n")
(close-input-port in)
(close-output-port out)
