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
  (define bs (make-bytes 1))
  (printf "Inside handle-server-msg!\n") (flush-output)
  (let till-empty ((bytes-avail (peek-bytes-avail!* bs 0 #f in)))
    (printf "Just peeked `~a' bytes\n" bytes-avail) (flush-output)
    (when (not (= 0 bytes-avail))
      (if (string=? "" (string-trim (bytes->string/latin-1 bs)))
	(begin (printf "About to read-string in handle-server-msg!\n") (flush-output)
	       (read-string 1 in))
	(begin (printf "About to read form\n") (flush-output)
	       (process-server-msg (read in))))

      (printf "Calling till-empty...\n") (flush-output)
      (till-empty (peek-bytes-avail!* bs 0 #f in)))))

; TODO!!!!! Same problem with peek-string as with byte-ready? !!!! hangs.
; TODO: Use one of the avail methods instead...
    ; TODO: Try using peek-bytes-avail!*

(define (handle-rdy-to-send p)
  (printf "About to peek-string...\n") (flush-output)
  (let [(ps (string-trim (peek-string 1 0 p)))]
    (printf "Just did peek-string...\n") (flush-output)
    (if (string=? "" ps)
      ; Discard peeked whitespace
      (begin (printf "About to read-string in handle-rdy-to-send\n") (flush-output)
	     (read-string 1 p)
	     (printf "Just read-string\n") (flush-output))
      (begin (printf "Ready to send!\n") (flush-output)
	     (write (read p) out)
	     (printf "Just sent\n") (flush-output)
	     ; Caveat: Display (don't write) whitespace to delimit the datum for server read.
	     (display " " out)
	     (flush-output out))))
  ; TODO: Weird!!! byte-ready? is blocking!!!!!!!!!!
  (printf "Leaving handle-rdy-to-send\n") (flush-output))

(define server-msg-evt (wrap-evt in handle-server-msg))
(define rdy-to-send-evt (wrap-evt (standard-input-port) handle-rdy-to-send))

(let loop ((cntr 0))
  (when (= 0 (modulo cntr 1000000))
    (printf "cntr=~a\n" cntr))
  ; TODO: Go back to using the evt defined above.
  (printf "About to sync...\n") (flush-output)
  ; RACKET TCP BUG!!!!!!!!!!!!!!!!!
  ; Even sync/timeout is blocking!!!!! The only thing that can unblock is a new
  ; line on stdin (rdy-to-send-evt); once that occurs, the server-msg-evt will be
  ; ready next time we sync (and the extra call to handle-server-msg isn't needed).
  (sync/timeout 1 rdy-to-send-evt server-msg-evt)
  (printf "timed out?...\n") (flush-output)
  ; TODO: DEBUG
  (handle-server-msg in)
  (loop (+ cntr 1)))


(printf "Closing connection.\n")
(close-input-port in)
(close-output-port out)
