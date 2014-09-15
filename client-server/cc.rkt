#lang racket/base

(require racket/tcp)
(require racket/string) ; string-trim

(printf "Trying to connect to server... ")
(define-values (in out) (tcp-connect "localhost" 12346))
(printf "Ok.\n")

(define (process-server-msg msg)
  (printf "Client received: `~a'\n" msg)
  (flush-output))

; TODO: Fix this - like server!
; Eventually, move into common module...
(define (handle-server-msg in)
  (let till-empty [(br (byte-ready? in))]
    (when br
      (if (string=? "" (string-trim (peek-string 1 0 in)))
	(read-string 1 in)
	(process-server-msg (read in)))
      (till-empty (byte-ready? in)))))

; Note: in within this function is stdin in - NOT tcp in from server.
(define (handle-rdy-to-send in)
  (let till-empty [(br (byte-ready? in))]
    (when br
      (if (string=? "" (string-trim (peek-string 1 0 in)))
	; Discard peeked whitespace
	(begin (read-string 1 in))
	(begin (write (read in) out)
	       ; Caveat: Display (don't write) whitespace to delimit the datum
	       ; for server read.
	       (display " " out)
	       (flush-output out))))))

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
