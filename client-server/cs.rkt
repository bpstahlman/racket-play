
#lang racket/base

(require racket/tcp)

(define client-ios '())

(define (handle-new-client io)
  ; TODO: Send quit to old...
  ; Add '(in out) to list of clients
  (set! client-ios (cons io client-ios))
  (printf "Got new client connection!\n"))

(define (handle-client-msg port)
  (printf "\nGot new msg from client:\n")
  (write (read port))
  (flush-output))

(define listener (tcp-listen 12346))
(define new-client-evt (wrap-evt (tcp-accept-evt listener) handle-new-client))

(let loop ()
  ; Note: sync res for new-client-evt is actually 2 el list of in out
  (apply sync new-client-evt
	 (map (lambda (io) (wrap-evt (car io) handle-client-msg)) client-ios))

  (loop)

  )

(tcp-close listener)
