#lang racket/base
;; Failure-taxonomy logger (catalog E1, note 04): what actually goes wrong when agents use steer,
;; recorded as it happens and classified, so the next tool to build is chosen by the largest
;; remaining class (note 04's decision rule) instead of by guesses.
;;   - main.rkt calls record-reply!/record-exn! for every command's output (one place, no per-command code),
;;   - one line per finding in .steer/failures.rktd: (ts agent tool kind class severity file line message),
;;   - kept local (gitignored): it is measurement data, and messages are clipped, no file contents,
;;   - recording never fails a command and is skipped when there is no store or STEER_NO_LOG is set.
;;   steer failures [--since ISO-DATE] [--who AGENT] [--class C] [--by class|kind|tool|agent]
;; (`--agent` is the global "who is acting" flag, hence `--who` for the filter)
(require racket/list racket/string racket/file
         "common.rkt" "store.rkt")
(provide classify class-hint record-reply! record-exn! record-finding! read-failures summarize cmd-failures)

(define (failures-file root) (build-path (store-dir root) "failures.rktd"))

;; ---------------------------------------------------------------------------------------------
;; Taxonomy (note 04: unbound id, arity, syntax, idiom, semantic, timeout, tool misuse)

(define (classify tool kind)
  (define k (if (symbol? kind) (symbol->string kind) kind))
  (define t (if (symbol? tool) (symbol->string tool) tool))
  (cond
    [(member k '("unknown-id" "not-in-racket")) 'unbound-id]
    [(member k '("read-error" "unclosed" "unclosed-form" "extra-closer" "mismatched-closer" "no-lang"))
     (if (equal? t "import") 'plan-error 'syntax)]
    [(member k '("removed-export" "arity-narrowed" "keywords-changed" "kind-changed" "contract-changed")) 'api-break]
    [(member k '("check-failed" "test-failed" "test-failed-more")) 'check-failed]
    [(member k '("diag-failed" "diag-failed-more")) 'build-failed]
    [(member k '("stale-anchor" "dropped-dependency" "missing-dependency" "cycle" "id-collision" "duplicate-seq"
                 "conflict-marker" "corrupt-task" "id-mismatch" "stray-file" "stale-claim"))
     (if (equal? k "cycle") (if (equal? t "import") 'plan-error 'drift) 'drift)]
    [(member k '("unknown-keyword" "unknown-label" "duplicate-label" "bad-label" "bad-value" "missing-title"
                 "not-a-task" "expected-keyword" "missing-value" "self-dependency"))
     'plan-error]
    [(member k '("weak-modal" "no-observable" "verb-form" "bad-start" "missing-comma" "missing-then" "missing-the" "missing-period"
                 "missing-shall" "empty-criterion" "no-criteria" "open-ended" "and-or" "subjective" "hedge" "vague-quantity"
                 "bare-quantity" "passive"))
     'criteria-error]
    [(member k '("usage" "incomplete-checkpoint" "no-check" "blocked" "claimed" "bad-id" "unknown-task" "not-active"
                 "no-store" "locked" "bad-ref" "no-repo"))
     'tool-misuse]
    [(member k '("internal" "store-corrupt" "no-racket" "no-docs" "doc-worker" "api-worker" "timeout" "no-gh" "github")) 'tool-failure]
    [else 'other]))

;; Which catalog item a class points at (the decision rule made explicit)
(define (class-hint c)
  (case c
    [(syntax) "structural checks: `steer syntax` and the post-edit hook (T44/T45 for other languages)"]
    [(unbound-id) "name lookup: `steer doc` (B1); a binding checker (A2, T13) would catch these before running"]
    [(check-failed) "test output parsing so failures are readable (T47), and better checks"]
    [(build-failed) "the code never reached its tests: a build or import error (T48 diagnostics), or a binding checker (A2) to catch it earlier"]
    [(tool-misuse) "usage hints and skill text: agents are misusing steer itself"]
    [(plan-error) "import error messages and the plan format docs"]
    [(criteria-error) "the criteria templates: skill text, `steer help spec`, the observable-verb and lint rule lists"]
    [(drift) "anchors and doctor: plans going stale, store integrity"]
    [(api-break) "API lock workflow (F3)"]
    [(tool-failure) "steer bugs and environment problems: fix first"]
    [else "unclassified: add a rule to classify"]))

;; ---------------------------------------------------------------------------------------------
;; Recording (never raises)

(define (enabled?) (not (getenv "STEER_NO_LOG")))

(define (append-record! root rec)
  (with-handlers ([(λ (e) #t) void])
    (call-with-output-file (failures-file root) #:exists 'append
      (λ (o) (write rec o) (newline o)))))

(define (record-reply! r)
  (when (enabled?)
    (with-handlers ([(λ (e) #t) void])
      (define root (find-root #:required? #f))
      (when (and root (directory-exists? (store-dir root)) (not (equal? (reply-tool r) "failures")))
        (define tool (reply-tool r))
        (define fs (filter (λ (f) (memq (hash-ref f 'severity) '(error warning))) (reply-findings r)))
        (for ([f fs])
          (define kind (hash-ref f 'kind))
          (append-record! root (list (now-iso) (current-agent) tool kind (classify tool kind) (hash-ref f 'severity)
                                     (hash-ref f 'file #f) (hash-ref f 'line #f) (clip (one-line (hash-ref f 'message "")) 200))))
        (when (and (not (reply-ok? r)) (null? fs))
          (append-record! root (list (now-iso) (current-agent) tool 'failed 'other 'error #f #f (clip (one-line (reply-text r)) 200))))))))

(define (record-exn! e)
  (when (enabled?)
    (with-handlers ([(λ (x) #t) void])
      (define root (find-root #:required? #f))
      (when (and root (directory-exists? (store-dir root)))
        (append-record! root (list (now-iso) (current-agent) 'steer (exn:steer-kind e) (classify 'steer (exn:steer-kind e)) 'error #f #f
                                   (clip (one-line (exn-message e)) 200)))))))

;; for code paths that exit before the normal output (the post-edit hook)
(define (record-finding! tool f)
  (when (enabled?)
    (with-handlers ([(λ (e) #t) void])
      (define root (find-root #:required? #f))
      (when (and root (directory-exists? (store-dir root)))
        (define kind (hash-ref f 'kind))
        (append-record! root (list (now-iso) (current-agent) tool kind (classify tool kind) (hash-ref f 'severity)
                                   (hash-ref f 'file #f) (hash-ref f 'line #f) (clip (one-line (hash-ref f 'message "")) 200)))))))

;; ---------------------------------------------------------------------------------------------
;; Reading and summarising

;; record: hasheq ts agent tool kind class severity file line message
(define (read-failures root)
  (define f (failures-file root))
  (if (file-exists? f)
      (for/list ([d (with-handlers ([exn:fail? (λ (e) '())])
                      (call-with-input-file f (λ (in) (parameterize ([read-accept-reader #f] [read-accept-lang #f]) (for/list ([x (in-port read in)]) x)))))]
                 #:when (and (list? d) (= (length d) 9)))
        (for/hasheq ([k '(ts agent tool kind class severity file line message)] [v d]) (values k v)))
      '()))

(define (count-by recs key)
  (define h (for/fold ([h (hash)]) ([r recs]) (hash-update h (hash-ref r key) add1 0)))
  (sort (hash->list h) (λ (a b) (or (> (cdr a) (cdr b)) (and (= (cdr a) (cdr b)) (string<? (format "~a" (car a)) (format "~a" (car b))))))))

(define (summarize recs by)
  (list (count-by recs by)))

(define (cmd-failures argv)
  (define-values (_ o) (parse-args "failures" argv '(("--since" one) ("--who" one) ("--by" one) ("--class" one)) #:max 0
                                   #:usage "steer failures [--since ISO-DATE] [--who AGENT] [--class CLASS] [--by class|kind|tool|agent]"))
  (define root (find-root))
  (define by (string->symbol (opt-ref o 'by "class")))
  (unless (memq by '(class kind tool agent))
    (fail! 'usage (format "--by ~a" (opt-ref o 'by)) #:hint "class | kind | tool | agent"))
  (define since (opt-ref o 'since))
  (define all (read-failures root))
  (define recs
    (filter (λ (r) (and (or (not since) (string>=? (hash-ref r 'ts) since))
                        (or (not (opt-ref o 'who)) (equal? (format "~a" (hash-ref r 'agent)) (opt-ref o 'who)))
                        (or (not (opt-ref o 'class)) (equal? (format "~a" (hash-ref r 'class)) (opt-ref o 'class)))))
            all))
  (define rows (count-by recs by))
  (define by-class (count-by recs 'class))
  (define top-class (and (pair? by-class) (car (car by-class))))
  (define cap (or (current-limit) (if (current-full?) +inf.0 12)))
  (make-reply "failures"
              (if (null? recs)
                  (format "no failures recorded~a (recording: every steer command logs its error/warning findings to .steer/failures.rktd)"
                          (if (null? all) "" " for this filter"))
                  (string-join
                   (append
                    (list (format "~a failure record~a from ~a agent~a, ~a → ~a"
                                  (length recs) (plural (length recs))
                                  (length (remove-duplicates (map (λ (r) (hash-ref r 'agent)) recs))) (plural (length (remove-duplicates (map (λ (r) (hash-ref r 'agent)) recs))))
                                  (hash-ref (car recs) 'ts) (hash-ref (last recs) 'ts))
                          (format "by ~a: ~a" by (string-join (for/list ([r (take rows (min (length rows) (inexact->exact (min cap 1000))))]) (format "~a ~a" (car r) (cdr r))) " · ")))
                    (if (eq? by 'class) '()
                        (list (format "by class: ~a" (string-join (for/list ([r by-class]) (format "~a ~a" (car r) (cdr r))) " · "))))
                    (if top-class (list (format "largest class: ~a → ~a" top-class (class-hint top-class))) '())
                    (let ([recent (take-right recs (min 5 (length recs)))])
                      (cons "recent:" (for/list ([r recent]) (format "  ~a @~a ~a/~a~a: ~a" (hash-ref r 'ts) (hash-ref r 'agent) (hash-ref r 'tool) (hash-ref r 'kind)
                                                                      (if (hash-ref r 'file) (format " ~a" (hash-ref r 'file)) "") (hash-ref r 'message))))))
                   "\n"))
              (hasheq 'total (length recs) 'by (symbol->string by)
                      'counts (for/list ([r rows]) (hasheq 'key (format "~a" (car r)) 'count (cdr r)))
                      'classes (for/list ([r by-class]) (hasheq 'class (format "~a" (car r)) 'count (cdr r))))))
