
#lang racket/base

(require racket/tcp)
(require racket/string)

(struct client (username in out logged-in))
(define client-ios '())
(define clients (hash))

(define (handle-new-client io)
  ; Add '(in out) to list of clients
  ; Separately, maintain a hash keyed by the in port itself.
  (set! client-ios (cons io client-ios))
  (set! clients (hash-set clients
			  (car io)
			  (client "" (car io) (cdr io) #f)))
  (printf "Client connected!\n")
  (flush-output)
  ; Return ports as sync result
  io)

(define (handle-client-msg port)
  (printf "Client msg received: ~a!\n" (read port)) (flush-output)
  ; Leave "synchronization result" unchanged: i.e., return the port.
  port)

(define listener (tcp-listen 12346))
(define new-client-evt (wrap-evt (tcp-accept-evt listener) handle-new-client))

(let loop ()
  ; Note: sync res for new-client-evt is actually 2 el list of in out
  (apply sync new-client-evt
	 ;(map (lambda (c) (wrap-evt (client-in c) handle-client-msg)) (hash-values clients)))
	 (map (lambda (p) (wrap-evt p handle-client-msg)) (hash-keys clients)))

  (loop))

(printf "Closing listener...\n") (flush-output)
(tcp-close listener)
