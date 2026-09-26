#lang racket/base
;; Code-facing commands: `syntax` (A1), `dup` (F5), `api` (F3), and the post-edit hook check.
(require racket/list racket/string racket/file
         "common.rkt" "store.rkt" "srcread.rkt" "syntax-check.rkt" "dup.rkt" "api.rkt")
(provide cmd-syntax cmd-dup cmd-api post-edit-problems)

(define (project-root) (or (find-root #:required? #f) (simplify-path (current-directory))))

(define (display-path root p)
  (define r (rel-path root p))
  (if (regexp-match? #rx"^\\.\\." r) (path->string (simplify-path (path->complete-path p))) r))

;; ---------------------------------------------------------------------------------------------
;; syntax

(define (cmd-syntax argv)
  (define-values (files o) (parse-args "syntax" argv '(("--fix" bool)) #:min 1 #:usage "steer syntax FILE... [--fix]"))
  (define root (project-root))
  (define fixes (if (opt-ref o 'fix) (for/list ([f files] #:when (file-exists? f)) (cons f (fix-file! f (display-path root f)))) '()))
  (define results
    (for/list ([f files])
      (cond
        [(not (file-exists? f)) (list f #f (list (finding 'error 'no-file (format "no such file ~a" f) #:file f)))]
        [else
         (define-values (fs n lang) (check-source (file->text f) (display-path root f)))
         (list f n fs)])))
  (define all (append-map caddr results))
  (define errors (filter (λ (x) (eq? (hash-ref x 'severity) 'error)) all))
  (make-reply "syntax"
              (string-join (append
                            (for*/list ([fx fixes] [e (cdr fx)])
                              (format "fixed ~a: ~a (structure may still differ from what you meant: rerun the tests)" (car fx) (edit-summary e)))
                            (for/list ([r results] #:when (null? (filter (λ (x) (eq? (hash-ref x 'severity) 'error)) (caddr r))))
                              (format "ok ~a (~a form~a)" (car r) (cadr r) (plural (or (cadr r) 0)))))
                           "\n")
              (hasheq 'files (for/list ([r results]) (hasheq 'file (car r) 'forms (cadr r) 'ok (null? (filter (λ (x) (eq? (hash-ref x 'severity) 'error)) (caddr r))))))
              #:ok? (null? errors) #:findings all))

;; --fix: apply verified edits only (the file must read afterwards), at most 3 rounds. → edits applied
(define (fix-file! path shown)
  (let loop ([round 0] [applied '()])
    (define text (file->text path))
    (define-values (fs _n _l) (check-source text shown))
    (define e (for/first ([f fs] #:when (let ([e (hash-ref f 'edit #f)]) (and e (hash-ref e 'verified #f)))) (hash-ref f 'edit)))
    (cond
      [(or (not e) (>= round 3)) (reverse applied)]
      [else
       (call-with-output-file path #:exists 'truncate (λ (o) (write-string (apply-edit text e) o)))
       (loop (add1 round) (cons e applied))])))

(define (edit-summary e)
  (case (hash-ref e 'op)
    [(insert) (format "inserted ~a at line ~a col ~a" (hash-ref e 'text) (hash-ref e 'line) (hash-ref e 'col))]
    [(delete) (format "deleted `~a` at line ~a col ~a" (hash-ref e 'text) (hash-ref e 'line) (hash-ref e 'col))]
    [(replace) (format "replaced the closer at line ~a col ~a with `~a`" (hash-ref e 'line) (hash-ref e 'col) (hash-ref e 'text))]))

;; For `steer hook post-edit`: #f when fine or not a Racket file, else text for the model.
(define (post-edit-problems file)
  (and (racket-file? file) (file-exists? file)
       (let-values ([(fs n lang) (check-source (file->text file) file)])
         (define errors (filter (λ (x) (eq? (hash-ref x 'severity) 'error)) fs))
         (and (pair? errors)
              (string-append "steer syntax found structural problems in the file just edited:\n"
                             (string-join (map finding->text (take errors (min 3 (length errors)))) "\n"))))))

;; ---------------------------------------------------------------------------------------------
;; dup

(define ignored-dirs '("compiled" ".git" ".steer" "node_modules" "dist" "build" ".claude"))

(define (racket-files-under root paths)
  (remove-duplicates
   (for*/list ([p paths]
               [f (cond [(file-exists? p) (list (path->complete-path p))]
                        [(directory-exists? p)
                         (for/list ([f (in-directory p (λ (d) (not (member (let-values ([(_b n _d) (split-path d)]) (path->string n)) ignored-dirs))))]
                                    #:when (and (file-exists? f) (racket-file? f)))
                           (path->complete-path f))]
                        [else (fail! 'usage (format "no such file or directory ~a" p))])])
     (simplify-path f))))

(define (cmd-dup argv)
  (define-values (paths o) (parse-args "dup" argv '(("--min-size" one) ("--loose" bool))
                                       #:usage "steer dup [PATH...] [--min-size TOKENS] [--loose]"))
  (define root (project-root))
  (define min-size (let ([n (string->number (opt-ref o 'min-size "30"))])
                     (unless (exact-positive-integer? n) (fail! 'usage "--min-size needs a positive integer"))
                     n))
  (define files (racket-files-under root (if (null? paths) (list (path->string root)) paths)))
  (define-values (groups skipped)
    (find-clones (for/list ([f files]) (cons (display-path root f) (file->text f)))
                 #:min-size min-size #:loose? (opt-ref o 'loose)))
  (define cap (or (current-limit) (if (current-full?) +inf.0 8)))
  (define shown (if (> (length groups) cap) (take groups (inexact->exact cap)) groups))
  (define (member-text m)
    (format "~a:~a-~a~a" (hash-ref m 'file) (hash-ref m 'line) (hash-ref m 'end)
            (if (hash-ref m 'in #f) (format " (in ~a)" (hash-ref m 'in)) "")))
  (make-reply "dup"
              (string-join
               (append
                (list (format "~a clone group~a (≥~a tokens~a) in ~a file~a"
                              (length groups) (plural (length groups)) min-size (if (opt-ref o 'loose) ", literals ignored" "")
                              (length files) (plural (length files))))
                (for/list ([g shown] [i (in-naturals 1)])
                  (define ms (hash-ref g 'members))
                  (format "[~a] ~a tokens ×~a: ~a\n    ~a" i (hash-ref g 'tokens) (length ms)
                          (string-join (map member-text ms) " · ")
                          (hash-ref (car ms) 'preview)))
                (if (> (length groups) (length shown)) (list (format "(+~a smaller groups; --limit N or --full)" (- (length groups) (length shown)))) '())
                (for/list ([s skipped]) (format "skipped ~a: ~a" (car s) (cdr s))))
               "\n")
              (hasheq 'groups (for/list ([g shown])
                                (hasheq 'tokens (hash-ref g 'tokens)
                                        'members (for/list ([m (hash-ref g 'members)])
                                                   (hasheq 'file (hash-ref m 'file) 'line (hash-ref m 'line) 'end (hash-ref m 'end) 'in (hash-ref m 'in #f))))))
              #:next (if (null? groups) '() (list "extract a shared helper if the copies must change together; leave them if they merely look alike"))))

;; ---------------------------------------------------------------------------------------------
;; api

(define (cmd-api argv)
  (define-values (pos o) (parse-args "api" argv '(("--timeout" one)) #:min 1
                                     #:usage "steer api snapshot [MODULE...] | steer api diff [MODULE...] | steer api show [MODULE]"))
  (define root (find-root))
  (define action (car pos))
  (define given (for/list ([m (cdr pos)])
                  (unless (file-exists? m) (fail! 'usage (format "no such module file ~a" m)))
                  (display-path root m)))
  (define timeout (let ([n (string->number (opt-ref o 'timeout "120"))]) (if (and n (> n 0)) n 120)))
  (define lock (read-lock root))
  (define (describe mods)
    (define r (api-describe root mods #:timeout timeout))
    (values (for/hash ([(k v) (in-hash r)] #:when (eq? (car v) 'ok)) (values k (cadr v)))
            (for/list ([(k v) (in-hash r)] #:when (eq? (car v) 'error))
              (finding 'error 'load-failed (format "could not load: ~a" (car (string-split (cadr v) "\n"))) #:file k
                       #:fix "run `racket FILE` to see the full error"))))
  (case action
    [("snapshot")
     (define mods (cond [(pair? given) given]
                        [lock (sort (hash-keys lock) string<?)]
                        [else (fail! 'usage "no modules given and no existing api.lock"
                                     #:hint "steer api snapshot main.rkt lib/*.rkt  (the list is remembered)")]))
     (define-values (ok errs) (describe mods))
     (cond
       [(pair? errs) (make-reply "api" "snapshot not written: some modules failed to load" #:ok? #f #:findings errs)]
       [else
        (with-store-lock root (λ () (write-lock! root (if (and lock (pair? given)) (for/fold ([m lock]) ([(k v) (in-hash ok)]) (hash-set m k v)) ok))
                                 (append-event! root 'api-snapshot #f (string-join mods " "))))
        (make-reply "api"
                    (string-join (for/list ([m mods]) (format "~a: ~a export~a" m (length (hash-ref ok m)) (plural (length (hash-ref ok m))))) "\n")
                    (hasheq 'modules mods)
                    #:next (list "commit .steer/api.lock" "steer api diff"))])]
    [("diff")
     (unless lock (fail! 'no-lock "no .steer/api.lock yet" #:hint "steer api snapshot MODULE..."))
     (define mods (if (pair? given) given (sort (hash-keys lock) string<?)))
     (for ([m mods]) (unless (hash-ref lock m #f) (fail! 'usage (format "~a is not in api.lock" m) #:hint (format "steer api snapshot ~a" m))))
     (define-values (ok errs) (describe mods))
     (define fs (append errs (append* (for/list ([m mods] #:when (hash-ref ok m #f)) (api-diff m (hash-ref lock m) (hash-ref ok m))))))
     (define (count sev) (length (filter (λ (x) (eq? (hash-ref x 'severity) sev)) fs)))
     (make-reply "api"
                 (format "api diff over ~a module~a: ~a breaking/error, ~a to review, ~a compatible"
                         (length mods) (plural (length mods)) (count 'error) (count 'warning) (count 'info))
                 (hasheq 'breaking (count 'error) 'review (count 'warning) 'compatible (count 'info))
                 #:ok? (= 0 (count 'error)) #:findings fs
                 #:next (if (null? fs) '() (list "if the changes are intended: steer api snapshot")))]
    [("show")
     (unless lock (fail! 'no-lock "no .steer/api.lock yet" #:hint "steer api snapshot MODULE..."))
     (define mods (if (pair? given) given (sort (hash-keys lock) string<?)))
     (make-reply "api"
                 (string-join
                  (for/list ([m mods])
                    (string-append m ":\n"
                                   (string-join (for/list ([e (hash-ref lock m '())])
                                                  (format "  ~a ~a~a~a" (car e) (cadr e)
                                                          (if (caddr e) (format " arity ~a" (arity-text (caddr e))) "")
                                                          (if (list-ref e 4) (format " ~a" (list-ref e 4)) "")))
                                                "\n")))
                  "\n")
                 (hasheq 'modules mods))]
    [else (fail! 'usage (format "unknown api action ~a" action) #:hint "snapshot | diff | show")]))
