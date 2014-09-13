
#lang racket/base

(require racket/tcp)
(require racket/string)

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
	(printf "Inside!!!! msg=~a\n" msg) (flush-output)
	(let ((out (client-out o-cli)))
	  (write msg out)
	  (display " " out)
	  (flush-output out)
	  (printf "Wrote and flushed msg to client ~a\n" out) (flush-output))))

; TODO: Consider turning this into a read-and-process-datum function, which takes only in port.
(define (process-client-msg msg cli)
  (define out (client-out cli))
  (case (car msg)
    ; Handle client login
    [(login)
     (printf "Got login request for ~a\n" (cadr msg)) (flush-output)
     (let* [(name (cadr msg))
	    ; Recall name starts out #f before login.
	    (rsp (not (findf (lambda (c) (and (client-name c)
					      (string=? (client-name c) name))) (hash-values clients))))]
       (when rsp
	 (set-client-name! cli (cadr msg)))
       ; Ether way, send client response: accept or reject
       (write (list 'login rsp) out)
       (display " " out)
       (flush-output out)
       (printf "Wrote accept/reject back to client!\n")
       (flush-output))]
    [(msg)
     (bcast-client-msg msg cli)]
    [else
      (printf "Bad msg from client! ~a - ~a\n" msg (car msg))]))

(define (handle-client-msg in)
  (printf "Inside handle-client-msg!\n") (flush-output)
  ; Consider using peek-bytes-avail! here to ensure that strange activity on port doesn't cause us to hang.
  (define spc (peek-string 1 0 in))
  (printf "Just peeked string `~a'\n" spc) (flush-output)
  (if (string=? "" (string-trim spc))
    (begin (printf "About to read-string\n") (flush-output) (read-string 1 in)
	   (printf "Just read string\n") (flush-output)
	   (printf "byte-ready? ~a\n" (byte-ready? in)) (flush-output))
    (begin (printf "About to read form\n") (flush-output)
	   (process-client-msg (read in) (hash-ref clients in)))))

(define listener (tcp-listen 12346))
(define new-client-evt (wrap-evt (tcp-accept-evt listener) handle-new-client))

(let loop ()
  ; Note: sync res for new-client-evt is actually 2 el list of in out
  (apply sync new-client-evt
	 (map (lambda (p) (wrap-evt p handle-client-msg)) (hash-keys clients)))

  (loop))

(printf "Closing listener...\n") (flush-output)
(tcp-close listener)
