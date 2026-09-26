#lang racket/base
;; Task-tracker commands (catalog F1/F2): the CLI surface over store.rkt, tasks.rkt, anchors.rkt.
;; Every mutation happens under the store lock and appends an event; output is budgeted.
(require racket/list racket/string racket/port
         "common.rkt" "store.rkt" "tasks.rkt" "anchors.rkt" "checks.rkt" "plan.rkt" "skills.rkt")
(provide cmd-init cmd-add cmd-import cmd-list cmd-show cmd-ready cmd-next cmd-claim cmd-release
         cmd-note cmd-checkpoint cmd-done cmd-verify cmd-drop cmd-reopen cmd-edit
         cmd-resume cmd-since cmd-graph cmd-stale cmd-refresh
         resume-text)

(define packet-budget 2000)

;; ---------------------------------------------------------------------------------------------
;; Shared helpers

(define (load-all root) (let ([ts (load-tasks root)]) (values ts (index ts))))

(define (get-task by-id raw)
  (define id (normalize-id raw))
  (or (hash-ref by-id id #f)
      (fail! 'unknown-task (format "no task ~a" id)
             #:hint (did-you-mean id (hash-keys by-id) #:else "`steer list --status all` shows every task"))))

(define (log-entry kind seq . kvs)
  (apply hasheq 'kind kind 'seq seq 'ts (now-iso) 'agent (current-agent) kvs))

(define (add-log t e) (task-update t 'log (λ (l) (append l (list e))) '()))

;; Normalize "path#name" so paths are root-relative; refuse paths outside the project.
(define (normalize-anchor-ref root ref)
  (define-values (p name) (parse-anchor ref))
  (define rel (rel-path root p))
  (when (or (regexp-match? #rx"^\\.\\." rel) (absolute-path? rel))
    (fail! 'bad-anchor (format "anchor ~a is outside the project root ~a" ref root)))
  (if name (string-append rel "#" name) rel))

(define (anchor-reports root t)
  (for/list ([a (task-ref t 'anchors '())])
    (define cur (resolve-anchor root (hash-ref a 'ref)))
    (hash-set cur 'state (anchor-state a cur))))

(define (anchor-text r)
  (define where (if (hash-ref r 'found? #f)
                    (format " L~a-~a" (hash-ref r 'line) (hash-ref r 'end))
                    ""))
  (define st (hash-ref r 'state))
  (string-append (hash-ref r 'ref) where " "
                 (case st
                   [(ok) "ok"]
                   [(changed) "CHANGED since planned (review; then `steer refresh`)"]
                   [(missing) (format "MISSING (~a)" (hash-ref r 'problem ""))]
                   [(created) "created"]
                   [(pending) "not yet created"])
                 (if (eq? (hash-ref r 'method #f) 'heuristic) " ~heuristic" "")))

(define (claim-text t) (if (task-ref t 'claimed-by) (string-append " @" (task-ref t 'claimed-by)) ""))

(define (task-line t by-id)
  (define deps (task-ref t 'after '()))
  (format "~a ~a p~a ~a~a~a"
          (let ([id (task-ref t 'id)]) (string-append id (make-string (max 0 (- 3 (string-length id))) #\space)))
          (let ([s (symbol->string (derived-status t by-id))]) (string-append s (make-string (max 0 (- 7 (string-length s))) #\space)))
          (task-ref t 'priority 2)
          (clip (task-ref t 'title) 70)
          (if (null? deps) "" (string-append " ← " (string-join deps ",")))
          (if (eq? (task-ref t 'status) 'active) (claim-text t) "")))

(define (log-text e full?)
  (define c (if full? (λ (s) s) (λ (s) (clip (one-line s) 220))))
  (define head (format "#~a ~a ~a @~a: " (hash-ref e 'seq) (hash-ref e 'kind) (age-text (hash-ref e 'ts)) (hash-ref e 'agent)))
  (string-append
   head
   (case (hash-ref e 'kind)
     [(checkpoint) (c (string-append "did: " (hash-ref e 'did) " | next: " (hash-ref e 'next)
                                     (let ([q (hash-ref e 'questions '())])
                                       (if (null? q) "" (string-append " | open: " (string-join q "; "))))))]
     [(done) (if (hash-ref e 'verified #f)
                 (format "verified by ~a check~a" (length (hash-ref e 'checks '())) (plural (length (hash-ref e 'checks '()))))
                 (c (string-append "UNVERIFIED: " (hash-ref e 'reason ""))))]
     [(check-failed) (c (string-append "failed: " (string-join (hash-ref e 'failed '()) "; ")))]
     [else (c (hash-ref e 'text ""))])))

;; The resume/show packet: everything a fresh context needs for one task, within a budget.
(define (packet root t by-id #:full? [full? (current-full?)] #:log-n [log-n 3])
  (define id (task-ref t 'id))
  (define reports (anchor-reports root t))
  (define bl (blockers t by-id))
  (define lg (task-ref t 'log '()))
  (define shown-log (if full? lg (let ([n (length lg)]) (if (> n log-n) (drop lg (- n log-n)) lg))))
  (define lines
    (append
     (list (format "~a [~a~a · p~a] ~a" id (derived-status t by-id) (claim-text t) (task-ref t 'priority 2) (task-ref t 'title)))
     (if (task-ref t 'goal) (list (string-append "goal: " (if full? (task-ref t 'goal) (clip (one-line (task-ref t 'goal)) 400)))) '())
     (if (null? (task-ref t 'after '())) '()
         (list (string-append "after: " (string-join (for/list ([d (task-ref t 'after)])
                                                        (format "~a ~a" d (let ([u (hash-ref by-id d #f)]) (if u (task-ref u 'status) 'missing))))
                                                      ", "))))
     (if (null? (task-ref t 'checks '()))
         (list "check: none (closing needs `steer done ID --unverified REASON`; prefer adding one with `steer edit ID --add-check CMD`)")
         (for/list ([c (task-ref t 'checks)]) (string-append "check: " c)))
     (for/list ([r reports]) (string-append "anchor: " (anchor-text r)))
     (if (null? (task-ref t 'touches '())) '() (list (string-append "touch: " (string-join (task-ref t 'touches) " "))))
     (if (null? (task-ref t 'tags '())) '() (list (string-append "tags: " (string-join (map (λ (x) (format "~a" x)) (task-ref t 'tags)) " "))))
     (if (null? shown-log) '()
         (cons (if (< (length shown-log) (length lg)) (format "log (last ~a of ~a):" (length shown-log) (length lg)) "log:")
               (for/list ([e shown-log]) (string-append "  " (log-text e full?)))))))
  (define text (string-join lines "\n"))
  (values (if (and (not full?) (> (string-length text) packet-budget))
              (string-append (substring text 0 (- packet-budget 40)) (format "… (`steer show ~a --full`)" id))
              text)
          (hasheq 'task (task->data t by-id) 'anchors reports 'blockers bl)))

(define (task->data t by-id)
  (hash-set (hash-set t 'derived (derived-status t by-id)) 'log (task-ref t 'log '())))

(define (split-words s) (filter (λ (x) (not (string=? x ""))) (string-split s)))

(define (parse-priority s)
  (define n (and s (string->number s)))
  (cond [(not s) #f]
        [(and (exact-integer? n) (<= 0 n 9)) n]
        [else (fail! 'usage (format "--priority must be an integer 0-9, got ~s" s) #:hint "0 = most urgent, default 2")]))

;; ---------------------------------------------------------------------------------------------
;; init

(define (cmd-init argv)
  (define-values (_ o) (parse-args "init" argv '(("--skills" bool) ("--force" bool))))
  (define root (simplify-path (path->complete-path (or (current-root-override) (current-directory)))))
  (define existed? (directory-exists? (store-dir root)))
  (init-store! root)
  ;; record only the directory name: the event log is committed and may be published
  (unless existed? (with-store-lock root (λ () (append-event! root 'init #f (let-values ([(_b name _d) (split-path root)]) (path->string name))))))
  (define skill-lines
    (if (opt-ref o 'skills)
        (for/list ([r (install-skills! (build-path root ".claude" "skills") #:force? (opt-ref o 'force))])
          (format "skill ~a: ~a" (car r) (cdr r)))
        '()))
  (make-reply "init"
              (string-join (cons (if existed? (format "store already exists: ~a" (store-dir root))
                                     (format "created ~a (commit it; .lock is ignored)" (store-dir root)))
                                 skill-lines)
                           "\n")
              (hasheq 'root (path->string root) 'created (not existed?))
              #:next (list "steer add \"TITLE\" --check CMD" "steer import plan.rktd" "steer help")))

;; ---------------------------------------------------------------------------------------------
;; add / import

(define (cmd-add argv)
  (define-values (pos o)
    (parse-args "add" argv '(("--goal" one) ("--after" many) ("--check" many) ("--anchor" many)
                             ("--touch" many) ("--priority" one) ("--tag" many))
                #:min 1 #:usage "steer add \"TITLE\" [--goal S] [--after T1,T2] [--check CMD]... [--anchor path#name]... [--touch PATH]... [--priority 0-9] [--tag T]..."))
  (define title (string-trim (string-join pos " ")))
  (when (string=? title "") (fail! 'usage "title is empty"))
  (define root (find-root))
  (with-store-lock root
    (λ ()
      (define-values (tasks by-id) (load-all root))
      (define after (remove-duplicates (for/list ([d (opt-ids o 'after)]) (task-ref (get-task by-id d) 'id))))
      (define refs (map (λ (r) (normalize-anchor-ref root r)) (opt-list o 'anchor)))
      (define anchors (map (λ (r) (baseline-anchor root r)) refs))
      (define id (next-id tasks))
      (define t (hasheq 'id id 'title title 'status 'open 'priority (or (parse-priority (opt-ref o 'priority)) 2)
                        'goal (opt-ref o 'goal) 'after after 'checks (opt-list o 'check)
                        'anchors anchors 'touches (opt-list o 'touch) 'tags (opt-list o 'tag)
                        'claimed-by #f 'created (now-iso) 'log '()))
      (save-task! root t)
      (append-event! root 'add id title)
      (define by-id* (hash-set by-id id t))
      (make-reply "add"
                  (format "added ~a [~a] ~a" id (derived-status t by-id*) title)
                  (hasheq 'id id 'task (task->data t by-id*))
                  #:findings (append
                              (if (null? (task-ref t 'checks))
                                  (list (finding 'warning 'no-check "no --check: `done` will require --unverified REASON" #:task id
                                                 #:fix (format "steer edit ~a --add-check \"CMD\"" id)))
                                  '())
                              (for/list ([a anchors] #:unless (hash-ref a 'hash))
                                (finding 'info 'anchor-pending (format "anchor ~a does not exist yet (fine if this task creates it)" (hash-ref a 'ref)) #:task id)))
                  #:next (list (format "steer show ~a" id))))))

(define (cmd-import argv)
  (define-values (pos o) (parse-args "import" argv '(("--dry-run" bool)) #:min 1 #:max 1
                                     #:usage "steer import PLAN-FILE|- [--dry-run]   (see `steer help import` for the format)"))
  (define src (car pos))
  (define text (if (equal? src "-")
                   (port->string (current-input-port))
                   (if (file-exists? src) (call-with-input-file src port->string)
                       (fail! 'usage (format "no such file ~a" src)))))
  (define name (if (equal? src "-") "<stdin>" src))
  (define root (find-root))
  (define-values (specs perrs) (parse-plan text name))
  (cond
    [(and (pair? perrs) (null? specs)) (import-failed perrs)]
    [(null? specs) (make-reply "import" "plan contains no (task ...) forms" #:ok? #f)]
    [else
     (with-store-lock root
       (λ ()
         (define-values (tasks by-id) (load-all root))
         ;; resolve even when some forms had errors, so one round trip reports every problem
         (define-values (new rerrs labels) (resolve-plan specs tasks name))
         (cond
           [(or (pair? perrs) (pair? rerrs))
            (import-failed (sort (append perrs rerrs) < #:key (λ (f) (hash-ref f 'line 0))))]
           [else
            (define made
              (for/list ([n new])
                (define refs (map (λ (r) (normalize-anchor-ref root r)) (hash-ref n 'anchor-refs)))
                (define t (hash-set* (hash-remove n 'anchor-refs)
                                     'anchors (map (λ (r) (baseline-anchor root r)) refs)
                                     'claimed-by #f 'created (now-iso) 'log '()))
                t))
            (define by-id* (for/fold ([m by-id]) ([t made]) (hash-set m (task-ref t 'id) t)))
            (unless (opt-ref o 'dry-run)
              (for ([t made])
                (save-task! root t)
                (append-event! root 'add (task-ref t 'id) (task-ref t 'title))))
            (define id->label (for/hash ([(l i) (in-hash labels)]) (values i l)))
            (make-reply "import"
                        (string-join
                         (cons (format "~a ~a task~a from ~a" (if (opt-ref o 'dry-run) "would create" "imported")
                                       (length made) (plural (length made)) name)
                               (for/list ([t made])
                                 (define lab (hash-ref id->label (task-ref t 'id) #f))
                                 (string-append (task-line t by-id*) (if lab (format "  (#:id ~a)" lab) ""))))
                         "\n")
                        (hasheq 'created (map (λ (t) (task-ref t 'id)) made) 'labels labels 'dry_run (and (opt-ref o 'dry-run) #t))
                        #:findings (for/list ([t made] #:when (null? (task-ref t 'checks)))
                                     (finding 'warning 'no-check (format "~a has no #:check; `done` will need --unverified" (task-ref t 'id)) #:task (task-ref t 'id)))
                        #:next (list "steer graph" "steer next --claim"))])))]))

(define (import-failed errs)
  (make-reply "import" (format "plan rejected: ~a problem~a, nothing was written" (length errs) (plural (length errs)))
              (hasheq 'errors (length errs)) #:ok? #f #:findings errs))

;; ---------------------------------------------------------------------------------------------
;; Reading: list / show / ready / next / graph / since / resume

(define (cmd-list argv)
  (define-values (_ o) (parse-args "list" argv '(("--status" one) ("--tag" many))
                                   #:usage "steer list [--status open|ready|blocked|active|done|dropped|all] [--tag T]"))
  (define root (find-root))
  (define-values (tasks by-id) (load-all root))
  (define st (opt-ref o 'status "open"))
  (define valid '("open" "ready" "blocked" "active" "done" "dropped" "all"))
  (unless (member st valid)
    (fail! 'usage (format "unknown --status ~a" st) #:hint (did-you-mean st valid #:else (string-join valid "|"))))
  (define tags (opt-list o 'tag))
  (define shown
    (for/list ([t tasks]
               #:when (let ([d (derived-status t by-id)])
                        (case st
                          [("all") #t]
                          [("open") (memq d '(ready blocked active))]
                          [else (eq? d (string->symbol st))]))
               #:when (or (null? tags) (for/or ([g tags]) (member g (map (λ (x) (format "~a" x)) (task-ref t 'tags '()))))))
      t))
  (define counts (for/fold ([m (hasheq)]) ([t tasks]) (hash-update m (derived-status t by-id) add1 0)))
  (define cap (or (current-limit) (if (current-full?) +inf.0 40)))
  (define lines (map (λ (t) (task-line t by-id)) (if (> (length shown) cap) (take shown (inexact->exact cap)) shown)))
  (make-reply "list"
              (string-join
               (append (list (format "~a task~a: ~a ready, ~a blocked, ~a active, ~a done, ~a dropped (showing ~a: ~a)"
                                     (length tasks) (plural (length tasks))
                                     (hash-ref counts 'ready 0) (hash-ref counts 'blocked 0) (hash-ref counts 'active 0)
                                     (hash-ref counts 'done 0) (hash-ref counts 'dropped 0) st (length shown)))
                       lines
                       (if (> (length shown) (length lines)) (list (format "(+~a more; --limit N or --full)" (- (length shown) (length lines)))) '()))
               "\n")
              (hasheq 'counts counts 'tasks (map (λ (t) (hasheq 'id (task-ref t 'id) 'title (task-ref t 'title)
                                                              'status (derived-status t by-id) 'after (task-ref t 'after '())
                                                              'priority (task-ref t 'priority 2) 'claimed_by (task-ref t 'claimed-by)))
                                                 shown))))

(define (cmd-show argv)
  (define-values (pos _) (parse-args "show" argv '() #:min 1 #:max 1 #:usage "steer show ID [--full]"))
  (define root (find-root))
  (define-values (_t by-id) (load-all root))
  (define t (get-task by-id (car pos)))
  (define-values (text data) (packet root t by-id))
  (make-reply "show" text data))

(define (cmd-ready argv)
  (parse-args "ready" argv '() #:max 0)
  (define root (find-root))
  (define-values (_t by-id) (load-all root))
  (define r (ready-order by-id))
  (make-reply "ready"
              (if (null? r) "nothing is ready (see `steer graph` for what blocks work)"
                  (string-join (cons (format "~a ready (best first):" (length r)) (map (λ (t) (task-line t by-id)) r)) "\n"))
              (hasheq 'ready (map (λ (t) (task-ref t 'id)) r))))

(define (mine by-id)
  (sort (for/list ([t (hash-values by-id)]
                   #:when (and (eq? (task-ref t 'status) 'active) (equal? (task-ref t 'claimed-by) (current-agent))))
          t)
        < #:key (λ (t) (id-number (task-ref t 'id)))))

(define (cmd-next argv)
  (define-values (_ o) (parse-args "next" argv '(("--claim" bool)) #:max 0 #:usage "steer next [--claim]"))
  (define root (find-root))
  (define (go)
    (define-values (_t by-id) (load-all root))
    (define my (mine by-id))
    (cond
      [(pair? my)
       (define-values (text data) (packet root (car my) by-id))
       (make-reply "next" text data
                   #:findings (list (finding 'info 'already-active (format "you (@~a) already have ~a active; finish, checkpoint or release it first"
                                                                          (current-agent) (string-join (map (λ (t) (task-ref t 'id)) my) ","))
                                             #:task (task-ref (car my) 'id)))
                   #:next (list (format "steer done ~a" (task-ref (car my) 'id))
                                (format "steer checkpoint ~a --did \"...\" --next \"...\"" (task-ref (car my) 'id))))]
      [else
       (define r (ready-order by-id))
       (cond
         [(null? r)
          (make-reply "next" "nothing is ready" (hasheq 'id #f)
                      #:findings (graph-findings by-id)
                      #:next (list "steer list" "steer graph"))]
         [else
          (define t0 (car r))
          (define t (if (opt-ref o 'claim) (do-claim! root t0) t0))
          (define by-id* (hash-set by-id (task-ref t 'id) t))
          (define-values (text data) (packet root t by-id*))
          (make-reply "next" (string-append (if (opt-ref o 'claim) "claimed " "") text) data
                      #:next (if (opt-ref o 'claim)
                                 (list (format "steer done ~a" (task-ref t 'id))
                                       (format "steer checkpoint ~a --did \"...\" --next \"...\"" (task-ref t 'id)))
                                 (list (format "steer claim ~a" (task-ref t 'id)))))])]))
  (if (opt-ref o 'claim) (with-store-lock root go) (go)))

(define (do-claim! root t)
  (define t* (task-set t 'status 'active 'claimed-by (current-agent)))
  (save-task! root t*)
  (append-event! root 'claim (task-ref t 'id) (current-agent))
  t*)

(define (cmd-graph argv)
  (parse-args "graph" argv '() #:max 0)
  (define root (find-root))
  (define-values (tasks by-id) (load-all root))
  (define fs (graph-findings by-id))
  (define cyclic? (for/or ([f fs]) (eq? (hash-ref f 'kind) 'cycle)))
  (define ls (if cyclic? '() (layers by-id)))
  (define cp (if cyclic? '() (critical-path by-id)))
  (make-reply "graph"
              (string-join
               (append
                (list (format "~a task~a, ~a problem~a" (length tasks) (plural (length tasks)) (length fs) (plural (length fs))))
                (if (null? ls) '()
                    (list (string-append "open work by layer: "
                                         (string-join (for/list ([l ls] [i (in-naturals)]) (format "L~a ~a" i (string-join l " "))) " · "))))
                (if (null? cp) '() (list (format "critical path (~a): ~a" (length cp) (string-join cp " → ")))))
               "\n")
              (hasheq 'layers ls 'critical_path cp)
              #:ok? (not (for/or ([f fs]) (eq? (hash-ref f 'severity) 'error)))
              #:findings fs))

(define (event-line e)
  (define-values (seq ts agent kind task detail) (apply values e))
  (format "#~a ~a @~a ~a~a~a" seq (age-text ts) agent kind (if task (string-append " " task) "")
          (if (and detail (not (equal? detail ""))) (string-append " " (clip (one-line (format "~a" detail)) 100)) "")))

(define (cmd-since argv)
  (define-values (pos _) (parse-args "since" argv '() #:min 1 #:max 1 #:usage "steer since CURSOR   (cursor comes from `steer resume`)"))
  (define n (string->number (string-trim (car pos) "#")))
  (unless (exact-nonnegative-integer? n) (fail! 'usage (format "cursor must be a number, got ~s" (car pos))))
  (define root (find-root))
  (define evs (filter (λ (e) (> (car e) n)) (read-events root)))
  (define now (last-seq root))
  (define cap (or (current-limit) (if (current-full?) +inf.0 30)))
  (define shown (if (> (length evs) cap) (take-right evs (inexact->exact cap)) evs))
  (make-reply "since"
              (string-join (append (list (format "~a event~a since #~a; cursor now #~a" (length evs) (plural (length evs)) n now))
                                   (if (> (length evs) (length shown)) (list (format "(~a older omitted)" (- (length evs) (length shown)))) '())
                                   (map event-line shown))
                           "\n")
              (hasheq 'cursor now 'events (map (λ (e) (hasheq 'seq (list-ref e 0) 'ts (list-ref e 1) 'agent (list-ref e 2)
                                                             'kind (list-ref e 3) 'task (list-ref e 4) 'detail (list-ref e 5)))
                                               shown))))

;; The session-start packet. Also used by `steer hook session-start`.
(define (resume-text root)
  (define-values (tasks by-id) (load-all root))
  (define counts (for/fold ([m (hasheq)]) ([t tasks]) (hash-update m (derived-status t by-id) add1 0)))
  (define cursor (last-seq root))
  (define my (mine by-id))
  (define others (filter (λ (t) (and (eq? (task-ref t 'status) 'active) (not (member t my)))) tasks))
  (define ready (ready-order by-id))
  (define header (format "steer resume @~a · cursor #~a · ~a ready · ~a active · ~a blocked · ~a/~a done"
                         (current-agent) cursor (hash-ref counts 'ready 0) (hash-ref counts 'active 0)
                         (hash-ref counts 'blocked 0) (hash-ref counts 'done 0) (length tasks)))
  (define my-block
    (cond [(null? my) '()]
          [else
           (define-values (text _) (packet root (car my) by-id #:log-n 2))
           (append (list (string-append "YOUR ACTIVE TASK\n" text))
                   (for/list ([t (cdr my)]) (string-append "also active: " (task-line t by-id))))]))
  (define other-block (if (null? others) '() (cons "active (other agents):" (map (λ (t) (string-append "  " (task-line t by-id))) others))))
  (define ready-block (if (null? ready) '() (cons "ready (best first):" (map (λ (t) (string-append "  " (task-line t by-id))) (take ready (min 3 (length ready)))))))
  (define stale
    (for*/list ([t tasks] #:when (memq (task-ref t 'status) '(open active))
                [r (anchor-reports root t)] #:when (memq (hash-ref r 'state) '(changed missing)))
      (finding 'warning 'stale-anchor (anchor-text r) #:task (task-ref t 'id)
               #:fix (format "re-read the code, adjust the plan, then `steer refresh ~a`" (task-ref t 'id)))))
  (define findings (append (graph-findings by-id) stale))
  (define next
    (cond [(pair? my) (list (format "steer done ~a" (task-ref (car my) 'id))
                            (format "steer checkpoint ~a --did \"...\" --next \"...\"" (task-ref (car my) 'id))
                            (format "steer since ~a" cursor))]
          [(pair? ready) (list "steer next --claim" (format "steer since ~a" cursor))]
          [(null? tasks) (list "steer add \"TITLE\" --check CMD" "steer import PLAN")]
          [else (list "steer graph" "steer list --status all")]))
  (define body (string-join (append (list header) my-block other-block ready-block) "\n"))
  (values (if (and (not (current-full?)) (> (string-length body) (* 2 packet-budget)))
              (string-append (substring body 0 (- (* 2 packet-budget) 40)) "… (`steer resume --full`)")
              body)
          (hasheq 'cursor cursor 'counts counts 'mine (map (λ (t) (task-ref t 'id)) my)
                  'ready (map (λ (t) (task-ref t 'id)) ready))
          findings next))

(define (cmd-resume argv)
  (parse-args "resume" argv '() #:max 0)
  (define root (find-root))
  (define-values (text data findings next) (resume-text root))
  (make-reply "resume" text data #:findings findings #:next next))

;; ---------------------------------------------------------------------------------------------
;; Mutations: claim / release / note / checkpoint / done / verify / drop / reopen / edit

(define (mutate-task! tool raw-id f)
  (define root (find-root))
  (with-store-lock root
    (λ ()
      (define-values (_t by-id) (load-all root))
      (f root (get-task by-id raw-id) by-id))))

(define (cmd-claim argv)
  (define-values (pos o) (parse-args "claim" argv '(("--force" bool)) #:min 1 #:max 1 #:usage "steer claim ID [--force]"))
  (mutate-task! "claim" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (define bl (blockers t by-id))
      (define holder (task-ref t 'claimed-by))
      (cond
        [(memq (task-ref t 'status) '(done dropped))
         (make-reply "claim" (format "~a is ~a" id (task-ref t 'status)) #:ok? #f #:next (list (format "steer reopen ~a" id)))]
        [(and (eq? (task-ref t 'status) 'active) (equal? holder (current-agent)))
         (define-values (text data) (packet root t by-id))
         (make-reply "claim" (string-append "already yours: " text) data)]
        [(and (eq? (task-ref t 'status) 'active) (not (opt-ref o 'force)))
         (make-reply "claim" (format "~a is active @~a" id holder) #:ok? #f
                     #:findings (list (finding 'error 'claimed (format "~a is claimed by ~a" id holder) #:task id
                                               #:fix "pick another with `steer next`, or take over with --force if that agent is gone")))]
        [(and (pair? bl) (not (opt-ref o 'force)))
         (make-reply "claim" (format "~a is blocked" id) #:ok? #f
                     #:findings (for/list ([b bl]) (finding 'error 'blocked (format "~a waits for ~a (~a)" id (car b) (cadr b)) #:task id
                                                            #:fix (format "work on ~a first (`steer show ~a`)" (car b) (car b)))))]
        [else
         (define t* (do-claim! root t))
         (define-values (text data) (packet root t* (hash-set by-id id t*)))
         (make-reply "claim" (string-append "claimed " text) data
                     #:next (list (format "steer done ~a" id) (format "steer checkpoint ~a --did \"...\" --next \"...\"" id)))]))))

(define (cmd-release argv)
  (define-values (pos _) (parse-args "release" argv '() #:min 1 #:max 1))
  (mutate-task! "release" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (unless (eq? (task-ref t 'status) 'active) (fail! 'not-active (format "~a is ~a, not active" id (task-ref t 'status))))
      (save-task! root (task-set t 'status 'open 'claimed-by #f))
      (append-event! root 'release id #f)
      (make-reply "release" (format "released ~a; it is open again" id)))))

(define (cmd-note argv)
  (define-values (pos _) (parse-args "note" argv '() #:min 2 #:usage "steer note ID TEXT"))
  (define text (string-join (cdr pos) " "))
  (mutate-task! "note" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (define seq (append-event! root 'note id text))
      (save-task! root (add-log t (log-entry 'note seq 'text text)))
      (make-reply "note" (format "noted on ~a (#~a)" id seq) (hasheq 'seq seq)))))

(define (cmd-checkpoint argv)
  (define-values (pos o) (parse-args "checkpoint" argv '(("--did" one) ("--next" one) ("--question" many) ("--release" bool))
                                     #:min 1 #:max 1
                                     #:usage "steer checkpoint ID --did \"what is done\" --next \"the very next step\" [--question Q]... [--release]"))
  (define did (string-trim (opt-ref o 'did "")))
  (define nxt (string-trim (opt-ref o 'next "")))
  (when (or (string=? did "") (string=? nxt ""))
    (fail! 'incomplete-checkpoint
           (format "a checkpoint needs both --did and --next (missing: ~a)"
                   (string-join (append (if (string=? did "") '("--did") '()) (if (string=? nxt "") '("--next") '())) ", "))
           #:hint "--did: what is finished and verified; --next: the concrete next action, so a fresh context can continue"))
  (mutate-task! "checkpoint" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (define seq (append-event! root 'checkpoint id (clip nxt 100)))
      (define t1 (add-log t (log-entry 'checkpoint seq 'did did 'next nxt 'questions (opt-list o 'question))))
      (define t2 (if (and (opt-ref o 'release) (eq? (task-ref t 'status) 'active)) (task-set t1 'status 'open 'claimed-by #f) t1))
      (save-task! root t2)
      (make-reply "checkpoint"
                  (format "checkpoint #~a saved on ~a~a. Safe to clear context; `steer resume` restores it." seq id
                          (if (opt-ref o 'release) " and released" ""))
                  (hasheq 'seq seq)))))

(define (run-checks root t)
  (define timeout (config-ref root 'check-timeout 600))
  (for/list ([c (task-ref t 'checks '())]) (run-check c root timeout)))

(define (check-findings id results)
  (for/list ([r results] #:unless (hash-ref r 'ok))
    (finding 'error 'check-failed
             (format "`~a` ~a after ~as" (hash-ref r 'cmd)
                     (if (eq? (hash-ref r 'exit) 'timeout) "timed out" (format "exited ~a" (hash-ref r 'exit)))
                     (hash-ref r 'secs))
             #:task id #:detail (let ([tl (hash-ref r 'tail)]) (and (not (string=? tl "")) tl)))))

(define (cmd-done argv)
  (define-values (pos o) (parse-args "done" argv '(("--unverified" one) ("--force" bool)) #:min 1 #:max 1
                                     #:usage "steer done ID [--unverified \"why no check can prove it\"] [--force]"))
  (mutate-task! "done" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (define reason (opt-ref o 'unverified))
      (cond
        [(memq (task-ref t 'status) '(done dropped))
         (make-reply "done" (format "~a is already ~a" id (task-ref t 'status)))]
        [(and (eq? (task-ref t 'status) 'active) (not (equal? (task-ref t 'claimed-by) (current-agent))) (not (opt-ref o 'force)))
         (make-reply "done" (format "refused: ~a is claimed by ~a, not ~a" id (task-ref t 'claimed-by) (current-agent)) #:ok? #f
                     #:findings (list (finding 'error 'claimed (format "~a belongs to ~a" id (task-ref t 'claimed-by)) #:task id
                                               #:fix "pass the right --agent, or --force if that agent is gone")))]
        [(and (null? (task-ref t 'checks '())) (not reason))
         (make-reply "done" (format "refused: ~a has no acceptance check" id) #:ok? #f
                     #:findings (list (finding 'error 'no-check "done needs a passing check or an explicit --unverified reason" #:task id
                                               #:fix (format "steer edit ~a --add-check \"CMD\"  (or: steer done ~a --unverified \"REASON\")" id id))))]
        [else
         (define results (if reason '() (run-checks root t)))
         (define failed (filter (λ (r) (not (hash-ref r 'ok))) results))
         (cond
           [(pair? failed)
            (define seq (append-event! root 'check-failed id (string-join (map (λ (r) (hash-ref r 'cmd)) failed) "; ")))
            (save-task! root (add-log t (log-entry 'check-failed seq 'failed (map (λ (r) (hash-ref r 'cmd)) failed))))
            (make-reply "done" (format "not done: ~a of ~a check~a failed on ~a" (length failed) (length results) (plural (length results)) id)
                        (hasheq 'checks results) #:ok? #f
                        #:findings (check-findings id results)
                        #:next (list (format "steer verify ~a" id)))]
           [else
            (define seq (append-event! root 'done id (if reason (string-append "UNVERIFIED: " reason) (format "~a check~a passed" (length results) (plural (length results))))))
            (define entry (if reason
                              (log-entry 'done seq 'verified #f 'reason reason)
                              (log-entry 'done seq 'verified #t 'checks (map (λ (r) (hasheq 'cmd (hash-ref r 'cmd) 'secs (hash-ref r 'secs))) results))))
            (define t* (add-log (task-set t 'status 'done) entry))
            (save-task! root t*)
            (define by-id* (hash-set by-id id t*))
            (define unblocked (for/list ([u (hash-values by-id*)]
                                         #:when (and (member id (task-ref u 'after '())) (ready? u by-id*)))
                                (task-ref u 'id)))
            (make-reply "done"
                        (string-append (format "~a done (~a)" id (if reason "UNVERIFIED" (format "~a check~a passed" (length results) (plural (length results)))))
                                       (if (null? unblocked) "" (string-append "; now ready: " (string-join (sort unblocked < #:key id-number) " "))))
                        (hasheq 'checks results 'unblocked unblocked)
                        #:next (list "steer next --claim"))])]))))

(define (cmd-verify argv)
  (define-values (pos _) (parse-args "verify" argv '() #:min 1 #:max 1))
  (define root (find-root))
  (define-values (_t by-id) (load-all root))
  (define t (get-task by-id (car pos)))
  (define id (task-ref t 'id))
  (define results (run-checks root t))
  (define ok? (andmap (λ (r) (hash-ref r 'ok)) results))
  (make-reply "verify"
              (cond [(null? results) (format "~a has no checks" id)]
                    [else (string-join (for/list ([r results]) (format "~a ~a (~as)" (if (hash-ref r 'ok) "pass" "FAIL") (hash-ref r 'cmd) (hash-ref r 'secs))) "\n")])
              (hasheq 'checks results)
              #:ok? (and ok? (pair? results))
              #:findings (check-findings id results)))

(define (cmd-drop argv)
  (define-values (pos o) (parse-args "drop" argv '(("--reason" one)) #:min 1 #:max 1 #:usage "steer drop ID --reason \"why\""))
  (define reason (opt-ref o 'reason))
  (unless reason (fail! 'usage "drop needs --reason" #:hint "the reason is what a later agent reads instead of guessing"))
  (mutate-task! "drop" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (define seq (append-event! root 'drop id reason))
      (define t* (add-log (task-set t 'status 'dropped 'claimed-by #f) (log-entry 'drop seq 'text reason)))
      (save-task! root t*)
      (define deps (for/list ([u (hash-values by-id)] #:when (and (member id (task-ref u 'after '())) (memq (task-ref u 'status) '(open active)))) (task-ref u 'id)))
      (make-reply "drop" (format "dropped ~a" id)
                  #:findings (for/list ([d deps]) (finding 'warning 'dropped-dependency (format "~a waited for ~a" d id) #:task d
                                                           #:fix (format "steer edit ~a --rm-after ~a" d id)))))))

(define (cmd-reopen argv)
  (define-values (pos _) (parse-args "reopen" argv '() #:min 1 #:max 1))
  (mutate-task! "reopen" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (define seq (append-event! root 'reopen id #f))
      (save-task! root (add-log (task-set t 'status 'open 'claimed-by #f) (log-entry 'reopen seq 'text "reopened")))
      (make-reply "reopen" (format "reopened ~a" id)))))

(define edit-spec
  '(("--title" one) ("--goal" one) ("--priority" one)
    ("--add-after" many) ("--rm-after" many) ("--add-check" many) ("--rm-check" many)
    ("--add-anchor" many) ("--rm-anchor" many) ("--add-touch" many) ("--rm-touch" many)
    ("--add-tag" many) ("--rm-tag" many)))

(define (cmd-edit argv)
  (define-values (pos o) (parse-args "edit" argv edit-spec #:min 1 #:max 1
                                     #:usage "steer edit ID [--title S] [--goal S] [--priority N] [--add-/--rm-after|check|anchor|touch|tag X]..."))
  (when (= 0 (hash-count o)) (fail! 'usage "nothing to edit" #:hint "e.g. steer edit T3 --add-check \"raco test x.rkt\""))
  (mutate-task! "edit" (car pos)
    (λ (root t by-id)
      (define id (task-ref t 'id))
      (define (rm-from lst xs what)
        (for ([x xs]) (unless (member x lst) (fail! 'usage (format "~a has no ~a ~s" id what x) #:hint (format "current: ~s" lst))))
        (filter (λ (y) (not (member y xs))) lst))
      (define add-after (for/list ([d (opt-ids o 'add-after)]) (task-ref (get-task by-id d) 'id)))
      (define rm-after (map normalize-id (opt-ids o 'rm-after)))
      ;; --rm-check accepts the exact command or its 1-based position
      (define checks (task-ref t 'checks '()))
      (define rm-checks (for/list ([c (opt-list o 'rm-check)])
                          (define n (string->number c))
                          (if (and (exact-positive-integer? n) (<= n (length checks)) (not (member c checks))) (list-ref checks (sub1 n)) c)))
      (define t*
        (task-set t
                  'title (or (opt-ref o 'title) (task-ref t 'title))
                  'goal (or (opt-ref o 'goal) (task-ref t 'goal))
                  'priority (or (parse-priority (opt-ref o 'priority)) (task-ref t 'priority 2))
                  'after (remove-duplicates (append (rm-from (task-ref t 'after '()) rm-after "dependency") add-after))
                  'checks (append (rm-from checks rm-checks "check") (opt-list o 'add-check))
                  'anchors (append (let ([rm (map (λ (r) (normalize-anchor-ref root r)) (opt-list o 'rm-anchor))])
                                     (define refs (map (λ (a) (hash-ref a 'ref)) (task-ref t 'anchors '())))
                                     (rm-from refs rm "anchor")
                                     (filter (λ (a) (not (member (hash-ref a 'ref) rm))) (task-ref t 'anchors '())))
                                   (map (λ (r) (baseline-anchor root (normalize-anchor-ref root r))) (opt-list o 'add-anchor)))
                  'touches (append (rm-from (task-ref t 'touches '()) (opt-list o 'rm-touch) "touch") (opt-list o 'add-touch))
                  'tags (append (rm-from (map (λ (x) (format "~a" x)) (task-ref t 'tags '())) (opt-list o 'rm-tag) "tag") (opt-list o 'add-tag))))
      (when (member id (task-ref t* 'after)) (fail! 'self-dependency (format "~a cannot depend on itself" id)))
      (define cyc (find-cycle (hash-set by-id id t*)))
      (when cyc (fail! 'cycle (string-append "edit would create a cycle: " (string-join cyc " → ")) #:hint "nothing was changed"))
      (save-task! root t*)
      (append-event! root 'edit id (string-join (map (λ (k) (format "--~a" k)) (sort (hash-keys o) symbol<?)) " "))
      (define-values (text data) (packet root t* (hash-set by-id id t*)))
      (make-reply "edit" (string-append "edited " text) data))))

;; ---------------------------------------------------------------------------------------------
;; Anchors: stale / refresh (plan drift)

(define (cmd-stale argv)
  (parse-args "stale" argv '() #:max 0)
  (define root (find-root))
  (define-values (tasks by-id) (load-all root))
  (define checked 0)
  (define fs
    (for*/list ([t tasks] #:when (memq (task-ref t 'status) '(open active))
                [r (anchor-reports root t)]
                #:when (begin (set! checked (add1 checked)) #t)
                #:when (memq (hash-ref r 'state) '(changed missing)))
      (finding 'warning 'stale-anchor (anchor-text r) #:task (task-ref t 'id)
               #:fix (format "re-read the code, adjust ~a's plan if needed, then `steer refresh ~a`" (task-ref t 'id) (task-ref t 'id)))))
  (make-reply "stale" (format "~a anchor~a on open tasks checked, ~a stale" checked (plural checked) (length fs))
              (hasheq 'checked checked 'stale (length fs))
              #:ok? (null? fs) #:findings fs))

(define (cmd-refresh argv)
  (define-values (pos o) (parse-args "refresh" argv '(("--all" bool)) #:usage "steer refresh ID... | --all"))
  (when (and (null? pos) (not (opt-ref o 'all))) (fail! 'usage "give task ids or --all"))
  (define root (find-root))
  (with-store-lock root
    (λ ()
      (define-values (tasks by-id) (load-all root))
      (define targets (if (opt-ref o 'all)
                          (filter (λ (t) (and (memq (task-ref t 'status) '(open active)) (pair? (task-ref t 'anchors '())))) tasks)
                          (map (λ (p) (get-task by-id p)) pos)))
      (define changed
        (for/list ([t targets])
          (define new (for/list ([a (task-ref t 'anchors '())]) (baseline-anchor root (hash-ref a 'ref))))
          (save-task! root (task-set t 'anchors new))
          (append-event! root 'refresh (task-ref t 'id) (format "~a anchor~a" (length new) (plural (length new))))
          (task-ref t 'id)))
      (make-reply "refresh" (format "re-baselined anchors on ~a" (if (null? changed) "no tasks" (string-join changed " ")))
                  (hasheq 'refreshed changed)))))
