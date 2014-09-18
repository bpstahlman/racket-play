#lang racket/base
(require racket/tcp)
(require "client-server-common.rkt")

(provide client-start)

(define (client-start hostname port callback)
  (define is-active #t)
  (define cust (make-custodian))
  ; cust-box becomes ready when cust is shutdown.
  (define cust-box (make-custodian-box cust #t))
  (parameterize
    ([current-custodian cust])
    (define-values [in out] (tcp-connect hostname port))
    ; Create async channels.
    ; Reader thread writes forms to ach-rcv. Writer pulls them from ach-snd.
    (define ach-rcv (make-async-channel))
    (define ach-snd (make-async-channel))
    ; Reader thread
    (make-reader-thread in ach-rcv callback)
    ; Writer thread
    (make-writer-thread in ach-rcv callback)

    ; Control thread
    (lambda args
      (if (and is-active (not (sync/timeout 0 cust-box)))
	(begin
	  (printf "Received request from client-client: ~a" args))
	(begin
	  ; Ignore requests after shutdown.
	  (set! is-active #f)
	  #f))
    )
  )

(define (make-reader-thread in ach-rcv callback)
  ; TODO: Check for eof
  (thread
    (lambda ()
      (let loop ()
	(define msg (read in))
	(match msg
	       [eof-object
		 (callback 'shutdown)
		 ; TODO: Is this safe?
		 (printf "Shutting down custodian from within reader thread.\n") (flush-output)
		 (custodian-shutdown-all (current-custodian))]
	       [(list 'msg username _ ...)
		; TODO: Inspect cmd type...
		(printf "Got msg from ~a: ~a" username msg) (flush-output)
		(async-channel-put ach-rcv msg)
		(callback 'msg-received)]
	       [(list 'login success _ ...)
		(printf "Got login response from server: ~a" success) (flush-output)
		(callback (if success 'login-success 'login-fail))]
	       [else
		 (printf "Unsupported message from client: ~a" msg) (flush-output)])
	))))

