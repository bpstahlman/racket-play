
#lang racket/base

(require racket/tcp)
(require racket/string)

(struct client (username in out logged-in))
(define client-ios '())
(define clients (hash))

(define (handle-new-client io)
  (printf "Inside handle-new-client!!!!\n")
  ; TODO: Send quit to old...
  ; Add '(in out) to list of clients
  (set! client-ios (cons io client-ios))
  ; Question: Is it possible that the input ports are the same?!?!
  ; Note: It's as though there's really only 1 active connection being listened to...
  (set! clients (hash-set clients
			  (car io)
			  (client "" (car io) (cdr io) #f)))
  (printf "Client connected!\n")
  (printf "Keys ~a" (hash-keys clients))
  (printf "Values ~a" (hash-values clients))
  (flush-output))

(define (handle-client-msg port)
  (read port)
  (printf "Client msg received!\n") (flush-output) port)
  ;(let ((s (read port)))
  ;  (printf "\nGot new msg from client ~a:`" port) (flush-output)
  ;  (write s)
  ;  (printf "'\n")
  ;  (flush-output)))

(define listener (tcp-listen 12346))
(define new-client-evt (wrap-evt (tcp-accept-evt listener) handle-new-client))

(let loop ()
  ; Note: sync res for new-client-evt is actually 2 el list of in out
  (printf "Applying sync: ~a" clients) (flush-output)
  (apply sync new-client-evt
	 ;(map (lambda (c) (wrap-evt (client-in c) handle-client-msg)) (hash-values clients)))
	 (map (lambda (p) (wrap-evt p handle-client-msg)) (hash-keys clients)))

  (loop)

  )
(printf "Closing listener...\n") (flush-output)
(tcp-close listener)
