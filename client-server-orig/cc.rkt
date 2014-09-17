#lang racket/base

(require racket/tcp)
(require racket/string) ; string-trim
(require "client-server-common.rkt")

(printf "Trying to connect to server... ")
(define-values (in out) (tcp-connect "localhost" 12346))
(printf "Ok.\n")

(define (process-server-msg msg)
  (printf "Client received: `~a'\n" msg)
  (flush-output))

(define (handle-server-msg in)
  (handle-form-maybe
    in
    process-server-msg
    (lambda () (printf "Got eof from server!\n") (flush-output))))

; Note: in within this function is stdin in - NOT tcp in from server.
(define (handle-rdy-to-send in)
  (handle-form-maybe
    in
    (lambda (msg)
      (write msg out)
      ; Caveat: Display (don't write) whitespace to delimit the datum for
      ; server read.
      (display " " out)
      (flush-output out))
    (lambda ()
      ; TODO: Escape through some sort of continuation.
      (printf "Got eof from terminal!\n") (flush-output))))

(define server-msg-evt (wrap-evt in handle-server-msg))
; CAVEAT: Do NOT use r6rs (standard-input-port): at the time of this writing,
; there's a bug preventing it from working with sync.
(define rdy-to-send-evt (wrap-evt (current-input-port) handle-rdy-to-send))

(let loop ()
  (sync rdy-to-send-evt server-msg-evt)
  (loop))


(printf "Closing connection.\n")
(close-input-port in)
(close-output-port out)
