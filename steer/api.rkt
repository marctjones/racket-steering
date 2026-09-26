#lang racket/base
;; Public API lock for Racket modules (catalog F3). A worker process (the installed `racket`, not
;; this binary: it needs every collection, and a separate process can be killed on timeout)
;; instantiates each module and records its phase-0 exports: kind, arity, keywords, contract.
;; Contracts are read inside the module's own namespace: `value-contract` only recognises wrappers
;; made by the same instance of racket/contract (tested; from outside it returns #f).
(require racket/list racket/string racket/file racket/pretty racket/port
         "common.rkt" "checks.rkt")
(provide api-describe api-diff read-lock write-lock! arity-text)

(define worker-source #<<EOF
#lang racket/base
(require racket/list racket/vector)
(define out-file (vector-ref (current-command-line-arguments) 0))
(define (arity->datum a)
  (cond [(exact-nonnegative-integer? a) a]
        [(arity-at-least? a) (list '>= (arity-at-least-value a))]
        [(list? a) (map arity->datum a)]
        [else #f]))
(define (describe path)
  (with-handlers ([(λ (e) #t) (λ (e) (list 'error (if (exn? e) (exn-message e) (format "~s" e))))])
    (define mp (path->complete-path path))
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (dynamic-require mp #f)
      (define-values (vars stxs) (module->exports mp))
      (define (phase0 l) (let ([p (assv 0 l)]) (if p (map car (cdr p)) '())))
      (namespace-require 'racket/contract/base)
      (namespace-require `(prefix steer-api: (file ,(path->string mp))))
      (list 'ok
            (for/list ([n (sort (remove-duplicates (append (phase0 vars) (phase0 stxs))) symbol<?)])
              (define id (string->symbol (format "steer-api:~a" n)))
              (define info
                (with-handlers ([exn:fail? (λ (e) #f)])
                  (eval `(let ([v ,id])
                           (list (if (procedure? v) 'procedure 'value)
                                 (and (procedure? v) (procedure-arity v))
                                 (and (procedure? v) (call-with-values (λ () (procedure-keywords v)) list))
                                 (let ([c (value-contract v)]) (and c (format "~s" (contract-name c)))))))))
              (if info
                  (list n (car info) (arity->datum (cadr info)) (caddr info) (cadddr info))
                  (list n 'macro #f #f #f)))))))
(define results (for/list ([p (vector-drop (current-command-line-arguments) 1)]) (cons p (describe p))))
(call-with-output-file out-file #:exists 'truncate (λ (o) (write results o)))
EOF
  )

(define (shell-quote s) (string-append "'" (string-replace s "'" "'\\''") "'"))

;; → hash: module-path → (list 'ok exports) | (list 'error msg)
(define (api-describe root modules #:timeout [timeout 120])
  (define racket (or (getenv "STEER_RACKET") (let ([p (find-executable-path "racket")]) (and p (path->string p)))))
  (unless racket
    (fail! 'no-racket "the API tool needs an installed `racket` on PATH to load modules" #:hint "install Racket or set STEER_RACKET" #:code 3))
  (define dir (make-temporary-directory "steer-api~a"))
  (define worker (build-path dir "worker.rkt"))
  (define out (build-path dir "out.rktd"))
  (call-with-output-file worker (λ (o) (write-string worker-source o)))
  (define r (run-check (string-join (append (list (shell-quote racket) (shell-quote (path->string worker)) (shell-quote (path->string out)))
                                            (map shell-quote modules))
                                    " ")
                       root timeout))
  (define result
    (cond
      [(file-exists? out)
       (for/hash ([e (call-with-input-file out (λ (in) (parameterize ([read-accept-reader #f]) (read in))))])
         (values (car e) (cdr e)))]
      [else (fail! 'api-worker (format "API worker failed (~a): ~a" (hash-ref r 'exit) (hash-ref r 'tail))
                   #:hint "run the module directly with `racket FILE` to see the error" #:code 3)]))
  (delete-directory/files dir)
  result)

;; ---------------------------------------------------------------------------------------------
;; Lock file: ((version 1) (modules (("path" ((name kind arity keywords contract) ...)) ...)))

(define (lock-path root) (build-path root ".steer" "api.lock"))

(define (read-lock root)
  (define p (lock-path root))
  (and (file-exists? p)
       (let ([d (call-with-input-file p (λ (in) (parameterize ([read-accept-reader #f]) (read in))))])
         (for/hash ([m (cadr (assq 'modules d))]) (values (car m) (cadr m))))))

(define (write-lock! root mods)
  (define p (lock-path root))
  (call-with-output-file p #:exists 'truncate
    (λ (o) (parameterize ([pretty-print-columns 100])
             (pretty-write `((version 1) (modules ,(for/list ([k (sort (hash-keys mods) string<?)]) (list k (hash-ref mods k))))) o)))))

;; ---------------------------------------------------------------------------------------------
;; Diff and classification

(define (arity-text a)
  (cond [(not a) "-"]
        [(exact-integer? a) (number->string a)]
        [(and (pair? a) (eq? (car a) '>=)) (format "~a+" (cadr a))]
        [(list? a) (string-join (map arity-text a) "|")]
        [else (format "~a" a)]))

(define (accepts? a n)
  (cond [(not a) #f]
        [(exact-integer? a) (= a n)]
        [(and (pair? a) (eq? (car a) '>=)) (>= n (cadr a))]
        [(list? a) (for/or ([x a]) (accepts? x n))]
        [else #f]))

(define (unbounded? a)
  (cond [(and (pair? a) (eq? (car a) '>=)) #t]
        [(list? a) (ormap unbounded? a)]
        [else #f]))

;; Every call the old arity allowed must still be allowed.
(define (arity-narrowed? old new)
  (or (for/or ([n (in-range 0 64)]) (and (accepts? old n) (not (accepts? new n))))
      (and (unbounded? old) (not (unbounded? new)))))

(define (arity-widened? old new) (arity-narrowed? new old))

;; kws: (required allowed) where allowed #f = any keyword
(define (keywords-narrowed? old new)
  (and old new
       (or (for/or ([k (car new)]) (not (member k (car old))))                 ; new required keyword
           (and (cadr new) (or (not (cadr old)) (for/or ([k (cadr old)]) (not (member k (cadr new)))))))))

;; old, new: lists of (name kind arity keywords contract). → list of findings
(define (api-diff module old new)
  (define (by-name l) (for/hash ([e l]) (values (car e) e)))
  (define o (by-name old))
  (define n (by-name new))
  (define (f sev kind name msg [fix #f]) (finding sev kind (format "~a: ~a" name msg) #:file module #:fix fix))
  (append
   (for/list ([name (sort (hash-keys o) symbol<?)] #:unless (hash-ref n name #f))
     (f 'error 'removed-export name "export removed (breaking)" "restore it, or re-snapshot if the break is intended"))
   (for*/list ([name (sort (hash-keys o) symbol<?)]
               [oe (in-value (hash-ref o name))]
               [ne (in-value (hash-ref n name #f))]
               #:when ne
               [x (in-list
                   (let-values ([(ka aa kwa ca) (apply values (cdr oe))]
                                [(kb ab kwb cb) (apply values (cdr ne))])
                     (append
                      (if (not (eq? ka kb)) (list (f 'error 'kind-changed name (format "was a ~a, now a ~a (breaking)" ka kb))) '())
                      (cond [(and aa ab (arity-narrowed? aa ab))
                             (list (f 'error 'arity-narrowed name (format "arity ~a → ~a: some old calls now fail (breaking)" (arity-text aa) (arity-text ab))))]
                            [(and aa ab (arity-widened? aa ab))
                             (list (f 'info 'arity-widened name (format "arity ~a → ~a (compatible)" (arity-text aa) (arity-text ab))))]
                            [else '()])
                      (if (keywords-narrowed? kwa kwb)
                          (list (f 'error 'keywords-changed name (format "keywords ~s → ~s (breaking)" kwa kwb))) '())
                      (if (not (equal? ca cb))
                          (list (f 'warning 'contract-changed name (format "contract ~a → ~a (review compatibility)" (or ca "none") (or cb "none")))) '()))))])
     x)
   (for/list ([name (sort (hash-keys n) symbol<?)] #:unless (hash-ref o name #f))
     (f 'info 'added-export name "new export (compatible)"))))
