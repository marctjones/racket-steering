#lang racket/base
;; Running acceptance checks: shell commands in the project root with a time limit. The process
;; runs in its own group so a timeout kills the whole tree. Only the tail of the output is kept,
;; because the first thing a model needs is the final error, not the whole log.
(require racket/list racket/string racket/port)
(provide run-check tail-text)

(define keep-bytes 65536)

(define (run-check cmd dir timeout-secs)
  (define start (current-inexact-milliseconds))
  (define-values (p out in _err)
    (parameterize ([current-directory dir] [subprocess-group-enabled #t])
      (subprocess #f #f 'stdout "/bin/sh" "-c" cmd)))
  (close-output-port in)
  (define buf (box #""))
  (define reader
    (thread (λ ()
              (define chunk (make-bytes 4096))
              (let loop ()
                (define n (read-bytes-avail! chunk out))
                (unless (eof-object? n)
                  (define b (bytes-append (unbox buf) (subbytes chunk 0 n)))
                  (set-box! buf (if (> (bytes-length b) keep-bytes)
                                    (subbytes b (- (bytes-length b) keep-bytes))
                                    b))
                  (loop))))))
  (define finished? (sync/timeout timeout-secs p))
  (unless finished? (subprocess-kill p #t))
  (subprocess-wait p)
  (unless (sync/timeout 2 reader) (kill-thread reader))
  (close-input-port out)
  (define code (subprocess-status p))
  (hasheq 'cmd cmd
          'ok (and finished? (eqv? code 0))
          'exit (if finished? code 'timeout)
          'secs (/ (round (/ (- (current-inexact-milliseconds) start) 100.0)) 10.0)
          'tail (tail-text (bytes->string/utf-8 (unbox buf) #\?))))

;; Last lines, each clipped, within a byte budget.
(define (tail-text s #:lines [max-lines 20] #:chars [max-chars 1500])
  (define ls (filter (λ (l) (not (regexp-match? #px"^\\s*$" l)))
                     (string-split (regexp-replace* #rx"\e\\[[0-9;]*[A-Za-z]" s "") "\n")))
  (define tail (let ([n (length ls)]) (if (> n max-lines) (drop ls (- n max-lines)) ls)))
  (define clipped (for/list ([l tail]) (if (> (string-length l) 200) (string-append (substring l 0 199) "…") l)))
  (let loop ([ls clipped])
    (define t (string-join ls "\n"))
    (if (and (> (string-length t) max-chars) (pair? (cdr ls))) (loop (cdr ls)) t)))
