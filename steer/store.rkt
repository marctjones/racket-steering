#lang racket/base
;; The .steer/ store: one s-expression file per task (git-diffable, merge-friendly), an append-only
;; event log whose sequence numbers are the cursor for `steer since`, a config file, and an
;; exclusive lock around every mutation so several agents can share one store.
(require racket/list racket/string racket/file racket/pretty racket/path
         "common.rkt")
(provide find-root store-dir init-store! with-store-lock
         load-tasks load-task save-task! next-id normalize-id id-number
         parse-task-text delete-task! write-events! tasks-dir task-file-path
         task-ref task-set task-update
         append-event! read-events last-seq
         load-config config-ref save-config!
         rel-path)

(define store-name ".steer")

(define (store-dir root) (build-path root store-name))
(define (tasks-dir root) (build-path root store-name "tasks"))
(define (events-file root) (build-path root store-name "events.rktd"))
(define (config-file root) (build-path root store-name "config.rktd"))
(define (lock-file root) (build-path root store-name ".lock"))

;; ---------------------------------------------------------------------------------------------
;; Root discovery: --root / STEER_ROOT, else the nearest ancestor of cwd holding .steer/ (like git).

(define (find-root #:required? [required? #t])
  (define override (or (current-root-override) (getenv "STEER_ROOT")))
  (cond
    [override
     (define r (simplify-path (path->complete-path override)))
     (when (and required? (not (directory-exists? (store-dir r))))
       (fail! 'no-store (format "no ~a directory in ~a" store-name r) #:hint "run `steer init` there"))
     r]
    [else
     (let loop ([dir (simplify-path (current-directory))])
       (cond [(directory-exists? (build-path dir store-name)) dir]
             [else
              (define-values (parent _name _dir?) (split-path dir))
              (cond [(path? parent) (loop parent)]
                    [required?
                     (fail! 'no-store (format "no ~a directory in ~a or any parent" store-name
                                              (current-directory))
                            #:hint "run `steer init` at the project root")]
                    [else #f])]))]))

(define (rel-path root p)
  (define r (find-relative-path (simplify-path root) (simplify-path (path->complete-path p root))))
  (path->string r))

;; ---------------------------------------------------------------------------------------------
;; Init

(define default-config '((version 1) (check-timeout 600) (api-modules ())))

(define (init-store! root)
  (make-directory* (tasks-dir root))
  (unless (file-exists? (config-file root)) (write-rktd (config-file root) default-config))
  (unless (file-exists? (events-file root)) (call-with-output-file (events-file root) void))
  (define gi (build-path (store-dir root) ".gitignore"))
  (unless (file-exists? gi) (call-with-output-file gi (λ (o) (display ".lock\n*.tmp\nfailures.rktd\n" o)))))

;; ---------------------------------------------------------------------------------------------
;; Locking: exclusive, polled, with a clear failure instead of a hang.

(define (with-store-lock root thunk)
  (call-with-file-lock/timeout
   #f 'exclusive thunk
   (λ () (fail! 'locked (format "store ~a is locked by another steer process" (store-dir root))
                #:hint "retry in a moment; if no steer is running, delete .steer/.lock" #:code 3))
   #:lock-file (lock-file root) #:delay 0.02 #:max-delay 0.5))

;; ---------------------------------------------------------------------------------------------
;; Reading and writing s-expression data safely (no #lang / #reader evaluation).

(define (read-rktd path)
  (with-handlers ([exn:fail:read?
                   (λ (e) (fail! 'store-corrupt (format "cannot read ~a: ~a" path (exn-message e))
                                 #:hint "fix or restore the file from git" #:code 3))])
    (call-with-input-file path
      (λ (in) (parameterize ([read-accept-reader #f] [read-accept-lang #f]) (read in))))))

(define (write-rktd path datum)
  (define tmp (path-add-extension path #".tmp"))
  (call-with-output-file tmp #:exists 'truncate
    (λ (o) (parameterize ([pretty-print-columns 100]) (pretty-write datum o))))
  (rename-file-or-directory tmp path #t))

;; ---------------------------------------------------------------------------------------------
;; Tasks: in memory an immutable hasheq; on disk an alist in a fixed key order for stable diffs.

(define key-order '(id title status priority goal after checks anchors touches tags claimed-by
                       github created updated log))

(define (task-ref t k [default #f]) (hash-ref t k default))
(define (task-set t . kvs)
  (let loop ([t t] [kvs kvs])
    (if (null? kvs) t (loop (hash-set t (car kvs) (cadr kvs)) (cddr kvs)))))
(define (task-update t k f [default #f]) (hash-set t k (f (hash-ref t k default))))

(define (task->alist t)
  (define known (for/list ([k key-order] #:when (hash-has-key? t k)) (list k (encode (hash-ref t k)))))
  (define extra (for/list ([k (sort (filter (λ (k) (not (memq k key-order))) (hash-keys t))
                                    symbol<?)])
                  (list k (encode (hash-ref t k)))))
  (append known extra))

;; Nested hashes (anchors, log entries) are stored as alists too.
(define (encode v)
  (cond [(hash? v) (for/list ([k (sort (hash-keys v) symbol<?)]) (list k (encode (hash-ref v k))))]
        [(list? v) (map encode v)]
        [else v]))

(define (alist->hash a) (for/hasheq ([kv a]) (values (car kv) (cadr kv))))

(define (alist->task a path)
  (unless (and (list? a) (andmap (λ (kv) (and (list? kv) (= 2 (length kv)) (symbol? (car kv)))) a))
    (fail! 'store-corrupt (format "~a is not a task record" path) #:code 3))
  (define t (alist->hash a))
  (define t* (task-set t
                       'anchors (map alist->hash (task-ref t 'anchors '()))
                       'log (map alist->hash (task-ref t 'log '()))))
  ;; nested records added later (github mirror state) are alists on disk too
  (if (pair? (task-ref t* 'github #f)) (task-set t* 'github (alist->hash (task-ref t* 'github))) t*))

(define (task-file root id) (build-path (tasks-dir root) (string-append id ".rktd")))

(define (load-tasks root)
  (define dir (tasks-dir root))
  (define files (if (directory-exists? dir)
                    (filter (λ (p) (regexp-match? #rx"^T[0-9]+\\.rktd$" (path->string p)))
                            (directory-list dir))
                    '()))
  (sort (for/list ([f files]) (alist->task (read-rktd (build-path dir f)) f))
        < #:key (λ (t) (id-number (task-ref t 'id)))))

(define (load-task root id)
  (define f (task-file root id))
  (unless (file-exists? f)
    (define ids (map (λ (t) (task-ref t 'id)) (load-tasks root)))
    (fail! 'unknown-task (format "no task ~a" id)
           #:hint (did-you-mean id ids #:else "list tasks with `steer list --status all`")))
  (alist->task (read-rktd f) f))

(define (save-task! root t)
  (write-rktd (task-file root (task-ref t 'id)) (task->alist (task-set t 'updated (now-iso)))))

;; Parse the text of one task file (also used on `git show` output). Raises exn:steer when unreadable.
(define (parse-task-text text label)
  (define d (with-handlers ([exn:fail:read?
                             (λ (e) (fail! 'store-corrupt (format "cannot read ~a: ~a" label (car (string-split (exn-message e) "\n")))
                                           #:hint "fix or restore the file from git" #:code 3))])
              (parameterize ([read-accept-reader #f] [read-accept-lang #f]) (read (open-input-string text)))))
  (alist->task d label))

(define (task-file-path root id) (task-file root id))

(define (delete-task! root id)
  (define f (task-file root id))
  (when (file-exists? f) (delete-file f)))

;; Rewrite the whole event log (used by `steer doctor --fix` to resequence). Hold the store lock.
(define (write-events! root evs)
  (define tmp (path-add-extension (events-file root) #".tmp"))
  (call-with-output-file tmp #:exists 'truncate
    (λ (o) (for ([e evs]) (write e o) (newline o))))
  (rename-file-or-directory tmp (events-file root) #t))

;; Ids are T<n>. Accepts "T12", "t12", "12".
(define (normalize-id s)
  (define m (regexp-match #px"^[Tt]?([0-9]+)$" (string-trim (format "~a" s))))
  (unless m
    (fail! 'bad-id (format "~s is not a task id" s) #:hint "task ids look like T12"))
  (string-append "T" (number->string (string->number (cadr m)))))

(define (id-number id) (string->number (substring id 1)))

(define (next-id tasks)
  (format "T~a" (add1 (for/fold ([m 0]) ([t tasks]) (max m (id-number (task-ref t 'id)))))))

;; ---------------------------------------------------------------------------------------------
;; Event log: one datum per line: (seq ts agent kind task-id detail)

(define (read-events root)
  (define f (events-file root))
  (if (file-exists? f)
      (call-with-input-file f
        (λ (in)
          (parameterize ([read-accept-reader #f] [read-accept-lang #f])
            (for/list ([d (in-port read in)]) d))))
      '()))

(define (last-seq root)
  (define evs (read-events root))
  (if (null? evs) 0 (car (last evs))))

;; Call only while holding the store lock.
(define (append-event! root kind task-id detail)
  (define seq (add1 (last-seq root)))
  (call-with-output-file (events-file root) #:exists 'append
    (λ (o) (write (list seq (now-iso) (current-agent) kind task-id detail) o) (newline o)))
  seq)

;; ---------------------------------------------------------------------------------------------
;; Config

(define (load-config root)
  (define f (config-file root))
  (if (file-exists? f) (alist->hash (read-rktd f)) (alist->hash default-config)))

(define (config-ref root k [default #f]) (hash-ref (load-config root) k default))

(define (save-config! root cfg)
  (write-rktd (config-file root) (for/list ([k (sort (hash-keys cfg) symbol<?)]) (list k (hash-ref cfg k)))))
