#lang racket/base
;; Sample data for measurement (task T8). Everything lands in samples/, which is gitignored:
;; the corpora and eval sets are fetched and rebuilt, never committed.
;;
;;   racket scripts/samples.rkt fetch    network: pinned git clones + MultiPL-E rows
;;   racket scripts/samples.rkt build    offline: eval tasks + ~19% held-out split
;;   racket scripts/samples.rkt status   counts; exit 1 if the eval sets are missing
;;
;; Task kinds (note 04): T1 write a function from a spec; T2 the same where the reference solution
;; needs library functions beyond racket/base; T3 repair a seeded bug (paren or unbound name) so the
;; tests pass. T3 mutants of a held-out exercise are held out too, so nothing leaks across the split.
(require racket/list racket/string racket/file racket/port racket/system racket/pretty
         racket/runtime-path json setup/dirs
         "../steer/srcread.rkt")

(define-runtime-path here ".")
(define root (simplify-path (build-path here 'up)))
(define samples-dir (build-path root "samples"))
(define corpus-dir (build-path samples-dir "corpus"))
(define raw-dir (build-path samples-dir "raw"))
(define eval-dir (build-path samples-dir "eval"))
(define sources (call-with-input-file (build-path here "samples-sources.rktd") read))

(define (say fmt . args) (apply printf fmt args) (newline) (flush-output))
(define (run! . args)
  (unless (apply system* (find-executable-path (car args)) (cdr args))
    (error 'samples "command failed: ~a" (string-join args " "))))
(define (write-rktd! p d) (make-parent-directory* p)
  (call-with-output-file p #:exists 'truncate (λ (o) (parameterize ([pretty-print-columns 100]) (pretty-write d o)))))
(define (write-text! p s) (make-parent-directory* p) (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string s o)))))
(define (sha1-hex s) (hex (sha1-bytes (open-input-bytes (string->bytes/utf-8 s)))))

;; ---------------------------------------------------------------------------------------------
;; fetch

(define (repo-dir name) (build-path corpus-dir "github" (string-replace name "/" "-")))

(define (fetch-github name sha)
  (define dir (repo-dir name))
  (define head (and (directory-exists? (build-path dir ".git"))
                    (string-trim (with-output-to-string (λ () (parameterize ([current-directory dir]) (system* (find-executable-path "git") "rev-parse" "HEAD")))))))
  (cond
    [(equal? head sha) (say "  ~a @ ~a (already there)" name (substring sha 0 10))]
    [else
     (when (directory-exists? dir) (delete-directory/files dir))
     (make-directory* dir)
     (parameterize ([current-directory dir])
       (run! "git" "init" "-q")
       (run! "git" "remote" "add" "origin" (format "https://github.com/~a.git" name))
       (run! "git" "fetch" "-q" "--depth" "1" "origin" sha)
       (run! "git" "checkout" "-q" "FETCH_HEAD"))
     (say "  ~a @ ~a" name (substring sha 0 10))]))

(define (fetch-hf dataset config split expected)
  (define out (build-path raw-dir (string-append config ".jsonl")))
  (define rows
    (let loop ([offset 0] [acc '()])
      (define url (format "https://datasets-server.huggingface.co/rows?dataset=~a&config=~a&split=~a&offset=~a&length=100"
                          dataset config split offset))
      (define page
        (let retry ([n 0])
          (define body (with-output-to-string (λ () (system* (find-executable-path "curl") "-s" "-m" "60" url))))
          (define j (with-handlers ([exn:fail? (λ (e) #f)]) (string->jsexpr body)))
          (cond [(and (hash? j) (hash-ref j 'rows #f)) (map (λ (r) (hash-ref r 'row)) (hash-ref j 'rows))]
                [(< n 3) (sleep (* 2 (add1 n))) (retry (add1 n))]
                [else (error 'samples "could not fetch ~a" url)])))
      (if (null? page) (reverse acc) (loop (+ offset (length page)) (append (reverse page) acc)))))
  (unless (= (length rows) expected)
    (say "  warning: ~a has ~a rows, expected ~a" config (length rows) expected))
  (write-text! out (string-join (map jsexpr->string rows) "\n"))
  (say "  ~a/~a: ~a rows" dataset config (length rows))
  (list config (length rows) (sha1-hex (file->string out))))

(define (installation-files which)
  (define dirs (case which
                 [("collects") (list (find-collects-dir))]
                 [("pkgs") (list (find-pkgs-dir))]))
  (sort (for*/list ([d dirs] #:when (and d (directory-exists? d))
                    [f (in-directory d (λ (p) (not (regexp-match? #rx"/compiled$" (path->string p)))))]
                    #:when (and (file-exists? f) (racket-file? f)))
          (path->string f))
        string<?))

(define (fetch!)
  (define manifest
    (for/list ([s sources])
      (case (car s)
        [(github) (fetch-github (list-ref s 1) (list-ref s 2)) `(github ,(list-ref s 1) ,(list-ref s 2))]
        [(hf) `(hf ,@(fetch-hf (list-ref s 1) (list-ref s 2) (list-ref s 3) (list-ref s 4)))]
        [(installation)
         (define files (installation-files (list-ref s 1)))
         (write-text! (build-path corpus-dir (format "installation-~a.txt" (list-ref s 1))) (string-join files "\n"))
         (say "  installation ~a: ~a files (read in place)" (list-ref s 1) (length files))
         `(installation ,(list-ref s 1) ,(length files) ,(version))])))
  (write-rktd! (build-path samples-dir "MANIFEST.rktd")
               `((fetched ,(current-seconds))
                 (racket ,(version)) (sources ,manifest)))
  (say "fetched into ~a" samples-dir))

;; ---------------------------------------------------------------------------------------------
;; build

(define (holdout? base-id) (memv (string-ref (sha1-hex base-id) 0) '(#\0 #\1 #\2)))  ; 3/16 ≈ 19%

(define (task-dir base-id id) (build-path eval-dir (if (holdout? base-id) "holdout" "tasks") id))

(define (emit-task! base-id id meta files)
  (define dir (task-dir base-id id))
  (write-rktd! (build-path dir "task.rktd") (append `((id ,id) (base ,base-id)) meta))
  (for ([f files]) (write-text! (build-path dir (car f)) (cdr f))))

;; T1 from MultiPL-E: prompt is an unfinished program; tests are appended after the completion.
(define (build-multipl-e!)
  (for*/sum ([config '("humaneval-rkt" "mbpp-rkt")]
             [f (in-value (build-path raw-dir (string-append config ".jsonl")))]
             #:when (file-exists? f)
             [line (file->lines f)]
             #:unless (string=? line ""))
    (define r (string->jsexpr line))
    (define id (string-downcase (hash-ref r 'name)))
    (emit-task! id id
                `((kind T1) (source ,(string-append "nuprl/MultiPL-E " config)) (format multipl-e)
                  (stop-tokens ,(hash-ref r 'stop_tokens))
                  (run "program = prompt.rkt + completion + tests.rkt; run with `racket`"))
                (list (cons "prompt.rkt" (hash-ref r 'prompt)) (cons "tests.rkt" (hash-ref r 'tests))))
    1))

;; Identifiers that come from common libraries but not racket/base: a reference solution that uses
;; any of them makes the exercise a library-use (T2) task.
(define library-ids
  (let ([ns (make-base-namespace)])
    (define (exports mod)
      (parameterize ([current-namespace ns])
        (dynamic-require mod #f)
        (define-values (vars stxs) (module->exports mod))
        (for*/list ([l (list vars stxs)] [p l] #:when (eqv? (car p) 0) [e (cdr p)]) (car e))))
    (define base (for/hasheq ([x (exports 'racket/base)]) (values x #t)))
    (for*/hasheq ([m '(racket/list racket/string racket/set racket/math racket/vector racket/function
                        racket/match racket/format racket/hash racket/sequence racket/stream racket/dict)]
                  [x (exports m)] #:unless (hash-ref base x #f))
      (values x m))))

(define (datum-symbols d)
  (cond [(symbol? d) (list d)] [(pair? d) (append (datum-symbols (car d)) (datum-symbols (cdr d)))]
        [(vector? d) (append-map datum-symbols (vector->list d))] [else '()]))

;; T3 mutations. Unbound: names models plausibly write that Racket does not bind.
(define wrong-names
  '((string-contains? . string-contains) (string-prefix? . string-starts-with?) (hash-ref . hash-get)
    (string-join . string-concat) (string-split . split-string) (list-ref . nth) (foldl . fold)
    (add1 . 1+) (sub1 . -1+) (for/list . for-list) (string-upcase . upcase) (number->string . num->string)
    (exact->inexact . to-float) (hash-set . hash-put) (vector-ref . vector-get) (char-upcase . char-to-upper)
    (remove-duplicates . unique) (filter . select) (string-length . string-len) (reverse . list-reverse)))

(define (read-forms text) (let-values ([(fs _l _t) (read-racket-source text)]) fs))

;; every list form's closing bracket position (0-based index into text), innermost included
(define (closer-positions forms)
  (define acc '())
  (let walk ([x forms])
    (cond [(list? x) (for-each walk x)]
          [(syntax? x)
           (define l (syntax->list x))
           (when (and l (syntax-position x) (syntax-span x))
             (set! acc (cons (+ (sub1 (syntax-position x)) (sub1 (syntax-span x))) acc)))
           (define e (syntax-e x))
           (when (pair? e) (let loop ([e e]) (cond [(pair? e) (walk (car e)) (loop (cdr e))] [(syntax? e) (walk e)])))]))
  (sort acc <))

(define (ident-positions forms name)
  (define acc '())
  (let walk ([x forms])
    (cond [(list? x) (for-each walk x)]
          [(syntax? x)
           (define e (syntax-e x))
           (cond [(eq? e name) (set! acc (cons (sub1 (syntax-position x)) acc))]
                 [(pair? e) (let loop ([e e]) (cond [(pair? e) (walk (car e)) (loop (cdr e))] [(syntax? e) (walk e)]))])]))
  (sort acc <))

(define (line-of text idx) (position->line text (add1 idx)))

;; deterministic choice from a list, seeded by a string
(define (pick seed lst) (and (pair? lst) (list-ref lst (modulo (string->number (substring (sha1-hex seed) 0 8) 16) (length lst)))))

(define expand-ns (make-base-namespace))
(define (expands? text dir)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (parameterize ([current-namespace expand-ns] [read-accept-reader #t] [current-load-relative-directory dir])
      (expand (read-syntax 'mutant (open-input-string text)))
      #t)))

(define (build-exercism!)
  (define base (build-path (repo-dir "exercism/racket") "exercises" "practice"))
  (for/fold ([counts (hasheq)]) ([slug (sort (map path->string (directory-list base)) string<?)])
    (define dir (build-path base slug))
    (define ref-p (build-path dir ".meta" "example.rkt"))
    (define test-p (build-path dir (string-append slug "-test.rkt")))
    (define stub-p (build-path dir (string-append slug ".rkt")))
    (cond
      [(not (and (file-exists? ref-p) (file-exists? test-p))) counts]
      [else
       (define ref (file->string ref-p))
       (define instr (let ([p (build-path dir ".docs" "instructions.md")]) (if (file-exists? p) (file->string p) "")))
       (define forms (read-forms ref))
       (define libs (remove-duplicates (for*/list ([s (datum-symbols (map syntax->datum forms))] [m (in-value (hash-ref library-ids s #f))] #:when m) m)))
       (define id (string-append "exercism-" slug))
       (define kind (if (null? libs) 'T1 'T2))
       (define test-name (string-append slug "-test.rkt"))
       (define sol-name (string-append slug ".rkt"))
       ;; data files some tests read (e.g. grep's texts) travel with the task
       (define fixtures (for/list ([f (directory-list dir)]
                                   #:when (file-exists? (build-path dir f))
                                   #:unless (regexp-match? #rx"^\\.|\\.rkt$|^README" (path->string f)))
                          (cons (path->string f) (file->string (build-path dir f)))))
       (define common (append (list (cons "instructions.md" instr) (cons test-name (file->string test-p)) (cons "reference.rkt" ref))
                              fixtures))
       (emit-task! id id `((kind ,kind) (source "exercism/racket") (format exercism) (libraries ,libs)
                           (solution-file ,sol-name) (run ,(format "write ~a, then `raco test ~a`" sol-name test-name)))
                   (cons (cons sol-name (if (file-exists? stub-p) (file->string stub-p) "")) common))
       ;; T3: a paren mutant (delete a closer) and, when a mutable name occurs, an unbound-name mutant
       (define closers (filter (λ (i) (memv (string-ref ref i) '(#\) #\]))) (closer-positions forms)))
       (define paren-at (pick (string-append id "/paren") closers))
       (define n-paren
         (cond
           [paren-at
            (define broken (string-append (substring ref 0 paren-at) (substring ref (add1 paren-at))))
            (emit-task! id (string-append id "-repair-paren")
                        `((kind T3) (mutation paren) (deleted ,(string (string-ref ref paren-at)))
                                    (line ,(line-of ref paren-at)) (index ,paren-at)
                                    (source "exercism/racket") (format exercism) (solution-file ,sol-name)
                                    (run ,(format "repair ~a so that `raco test ~a` passes" sol-name test-name)))
                        (cons (cons sol-name broken) common))
            1]
           [else 0]))
       (define candidates (for*/list ([w wrong-names] [ps (in-value (ident-positions forms (car w)))] #:when (pair? ps)) (cons w ps)))
       (define choice (pick (string-append id "/name") candidates))
       (define n-name
         (cond
           [choice
            (define w (car choice))
            (define at (pick (string-append id "/at") (cdr choice)))
            (define old (symbol->string (car w)))
            (define broken (string-append (substring ref 0 at) (symbol->string (cdr w)) (substring ref (+ at (string-length old)))))
            (cond
              [(expands? broken dir) 0]              ; still compiles: not a useful repair task
              [else
               (emit-task! id (string-append id "-repair-name")
                           `((kind T3) (mutation unbound) (wrong ,(cdr w)) (right ,(car w)) (line ,(line-of ref at))
                                       (source "exercism/racket") (format exercism) (solution-file ,sol-name)
                                       (run ,(format "repair ~a so that `raco test ~a` passes" sol-name test-name)))
                           (cons (cons sol-name broken) common))
               1])]
           [else 0]))
       (hash-update (hash-update counts kind add1 0) 'T3 (λ (n) (+ n n-paren n-name)) 0)])))

(define (build!)
  (when (directory-exists? eval-dir) (delete-directory/files eval-dir))
  (define n-mpe (build-multipl-e!))
  (define ex (build-exercism!))
  (say "built ~a MultiPL-E T1 tasks; exercism: ~a T1, ~a T2, ~a T3 repair" n-mpe (hash-ref ex 'T1 0) (hash-ref ex 'T2 0) (hash-ref ex 'T3 0))
  (status #f))

;; ---------------------------------------------------------------------------------------------
;; status

(define (status require?)
  (define (count-kinds split)
    (define d (build-path eval-dir split))
    (if (directory-exists? d)
        (for/fold ([m (hasheq)]) ([t (directory-list d)])
          (define meta (call-with-input-file (build-path d t "task.rktd") read))
          (hash-update m (cadr (assq 'kind meta)) add1 0))
        #f))
  (define tasks (count-kinds "tasks"))
  (define holdout (count-kinds "holdout"))
  (define (fmt m) (if m (string-join (for/list ([k '(T1 T2 T3)]) (format "~a ~a" k (hash-ref m k 0))) ", ") "missing"))
  (say "eval/tasks:   ~a" (fmt tasks))
  (say "eval/holdout: ~a" (fmt holdout))
  (define corpus (for/list ([s sources] #:when (eq? (car s) 'github)) (list-ref s 1)))
  (say "corpus: ~a" (string-join (for/list ([c corpus]) (format "~a~a" c (if (directory-exists? (repo-dir c)) "" " (missing)"))) ", "))
  (when (and require? (not (and tasks holdout (> (hash-ref tasks 'T1 0) 0) (> (hash-ref holdout 'T1 0) 0) (> (hash-ref tasks 'T3 0) 0))))
    (say "eval sets missing or incomplete: run `make samples`")
    (exit 1)))

;; ---------------------------------------------------------------------------------------------
;; validate: every exercism reference must pass its tests; every T3 mutant must fail them.
;; (MultiPL-E has no reference solutions, so its tasks are only checked for well-formed tests.)

(define (raco-test-passes? task-path solution-text)
  (define tmp (make-temporary-directory "steer-validate~a"))
  (define meta (call-with-input-file (build-path task-path "task.rktd") read))
  (define sol (cadr (assq 'solution-file meta)))
  (for ([f (directory-list task-path)]
         #:unless (member (path->string f) (list "task.rktd" "reference.rkt" "instructions.md" sol)))
    (copy-file (build-path task-path f) (build-path tmp f)))
  (write-text! (build-path tmp sol) solution-text)
  (define ok? (parameterize ([current-directory tmp] [current-output-port (open-output-nowhere)] [current-error-port (open-output-nowhere)])
                (system* (find-executable-path "raco") "test" "-q" "--drdr" ".")))
  (delete-directory/files tmp)
  ok?)

(define (validate!)
  (define bad '())
  (define n 0)
  (for* ([split '("tasks" "holdout")]
         [t (directory-list (build-path eval-dir split))])
    (define p (build-path eval-dir split t))
    (define meta (call-with-input-file (build-path p "task.rktd") read))
    (when (eq? (cadr (assq 'format meta)) 'exercism)
      (set! n (add1 n))
      (define kind (cadr (assq 'kind meta)))
      (define sol (cadr (assq 'solution-file meta)))
      (define candidate (if (eq? kind 'T3) (file->string (build-path p sol)) (file->string (build-path p "reference.rkt"))))
      (define passes? (raco-test-passes? p candidate))
      (define ok? (if (eq? kind 'T3) (not passes?) passes?))
      (unless ok? (set! bad (cons (format "~a/~a: ~a" split t (if (eq? kind 'T3) "mutant still passes" "reference fails its tests")) bad)))))
  (say "validated ~a exercism tasks: ~a problem~a" n (length bad) (if (= 1 (length bad)) "" "s"))
  (for ([b (reverse bad)]) (say "  ~a" b))
  (unless (null? bad) (exit 1)))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (case (if (null? args) "status" (car args))
    [("fetch") (fetch!)]
    [("build") (build!)]
    [("validate") (validate!)]
    [("status") (status (member "--require" args))]
    [else (say "usage: racket scripts/samples.rkt fetch|build|status [--require]") (exit 2)]))
