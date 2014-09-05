
#lang racket/base

(require racket/tcp)
(require racket/string)

(define client-ios '())

(define listener (tcp-listen 12346))

(define (handle-new-client in out)
  ; Add '(in out) to list of clients
  (set! client-ios (cons (list in out) client-ios))
  (printf "Client connected!\n")
  (flush-output))

(define (try-accept-client)
  (when (tcp-accept-ready? listener)
    (let-values [((in out) (tcp-accept listener))]
		(handle-new-client in out))))

(define (try-rcv-client in)
  (when (byte-ready? in)
    (let [(bstr (make-bytes 100))]
      (printf "About to read from client...\n") (flush-output)
      (read-bytes-avail! bstr in)
      (printf "Got value `~a'\n" bstr) (flush-output)
      (printf "Done reading from client...\n") (flush-output))))

(let loop ()
  ; Note: sync res for new-client-evt is actually 2 el list of in out
  (try-accept-client)
  (for [(p client-ios)]
       (try-rcv-client (car p)))
  (loop))

(printf "Closing listener...\n") (flush-output)
(tcp-close listener)
