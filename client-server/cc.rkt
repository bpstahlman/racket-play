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
  ; TODO: byte-ready? test probably not necessary, given evt
  (let while-byte-ready ((br (and (sync/timeout 0 p) #t)))
    (printf "Inside while-byte-ready\n") (flush-output)
    (when br
      (printf "About to peek-string...\n") (flush-output)
      (let [(ps (string-trim (peek-string 1 0 p)))]
	(printf "Just did peek-string...\n") (flush-output)
	(if (string=? "" ps)
	  ; Discard peeked whitespace
	  (begin (printf "About to read-string\n") (flush-output)
		 (read-string 1 p)
		 (printf "Just read-string\n") (flush-output))
	  (begin (printf "Ready to send!\n") (flush-output)
		 (write (read p) out)
		 (printf "Just sent\n") (flush-output)
		 ; Caveat: Display (don't write) whitespace to delimit the datum for server read.
		 (display " " out)
		 (flush-output out))))
      (printf "About to call while-byte-ready with byte-ready?\n") (flush-output)
      ; TODO: Weird!!! byte-ready? is blocking!!!!!!!!!!
      (while-byte-ready (and (sync/timeout 0 p) #t))))
  (printf "Leaving handle-rdy-to-send\n") (flush-output))

(define server-msg-evt (wrap-evt in handle-server-msg))
(define rdy-to-send-evt (wrap-evt (standard-input-port) handle-rdy-to-send))

(let loop ((cntr 0))
  (when (= 0 (modulo cntr 1000000))
    (printf "cntr=~a\n" cntr))
  ; TODO: Go back to using the evt defined above.
  (printf "About to sync...\n") (flush-output)
  (sync rdy-to-send-evt (wrap-evt (standard-input-port) handle-rdy-to-send))
  (loop (+ cntr 1)))


(printf "Closing connection.\n")
(close-input-port in)
(close-output-port out)
