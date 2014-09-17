
#lang racket/base

(require racket/tcp)
(require racket/string)
(require "client-server-common.rkt")

(struct client (name in out) #:mutable)
(define clients (hash))

(define (handle-new-client io)
  ; Maintain a hash keyed by the in port itself.
  (set! clients (hash-set clients
			  (car io)
			  (client #f (car io) (cadr io))))
  (printf "Client connected!\n")
  (flush-output))

; Send to all logged-in clients but the specified one.
(define (bcast-client-msg msg cli)
  (for* [(o-cli (hash-values clients))
	 #:when (and (not (eq? cli o-cli)) (client-name o-cli))]
	(let ((out (client-out o-cli)))
	  (write msg out)
	  (display " " out)
	  (flush-output out))))

; TODO: Consider turning this into a read-and-process-datum function, which takes only in port.
(define (process-client-msg msg cli)
  (define out (client-out cli))
  (case (car msg)
    ; Handle client login
    [(login)
     (printf "Received login request for ~a\n" (cadr msg)) (flush-output)
     (let* [(name (cadr msg))
	    ; See whether this name is already taken.
	    ; Recall name starts out #f before login.
	    (rsp (not (findf (lambda (c) (and (client-name c)
					      (string=? (client-name c) name))) (hash-values clients))))]
       (when rsp
	 ; Login available
	 (set-client-name! cli (cadr msg)))
       ; Ether way, send client response: accept or reject
       (write (list 'login rsp) out)
       (display " " out)
       (flush-output out))]
    [(msg)
     (bcast-client-msg msg cli)]
    [else
      (printf "Unsupported msg from client! ~a\n" msg)]))

;; TODO: Under Construction!!!! Handler args...
(define (handle-client-msg in)
  (handle-form-maybe
    in
    (lambda (msg) (process-client-msg msg (hash-ref clients in)))
    ; TODO: Discard this client...
    (lambda () (printf "Got eof from client!\n") (flush-output))))


(define listener (tcp-listen 12346))
(define new-client-evt (wrap-evt (tcp-accept-evt listener) handle-new-client))

(let loop ()
  ; Note: sync res for new-client-evt is actually 2 el list of in out
  (apply sync new-client-evt
	 (map (lambda (p) (wrap-evt p handle-client-msg)) (hash-keys clients)))

  (loop))

(printf "Closing listener...\n") (flush-output)
(tcp-close listener)
