#lang racket/base
;; One-way mirror of tagged tasks to GitHub milestones and issues (catalog F8).
;; .steer/ stays the source of truth: agents keep using steer; humans get issues on GitHub.
;;   - a selected task tagged `milestone` becomes a GitHub milestone,
;;   - every other selected task becomes an issue, assigned to the milestone task that lists it in
;;     #:after, labelled with its tags, with goal, checks, dependencies and anchors in the body,
;;   - done/dropped closes the issue (completed / not planned) and the milestone,
;;   - the GitHub number and a digest of what was sent are stored in the task, so a re-run only
;;     sends what changed.
;; Planning is pure (sync-plan); execution shells out to `gh api` (STEER_GH overrides, for tests).
(require racket/list racket/string racket/port racket/system json
         "common.rkt" "store.rkt" "tasks.rkt" "srcread.rkt")
(provide sync-plan render-issue-body issue-desired milestone-desired cmd-github)

(define (tags-of t) (map (λ (x) (format "~a" x)) (task-ref t 'tags '())))
(define (milestone-task? t) (member "milestone" (tags-of t)))
(define (gh-field t k) (let ([g (task-ref t 'github #f)]) (and (hash? g) (hash-ref g k #f))))
(define (closed? t) (memq (task-ref t 'status) '(done dropped)))
(define (digest . parts) (substring (hex (sha1-bytes (open-input-bytes (string->bytes/utf-8 (format "~s" parts))))) 0 12))

;; ---------------------------------------------------------------------------------------------
;; Rendering

(define (ref-text by-id id)
  (define u (hash-ref by-id id #f))
  (define n (and u (gh-field u 'number)))
  (define kind (and u (gh-field u 'kind)))
  (cond [(and n (eq? kind 'issue)) (format "~a (#~a)" id n)]
        [else id]))

(define (render-issue-body t by-id)
  (define id (task-ref t 'id))
  (string-join
   (filter values
           (list (or (task-ref t 'goal) "(no goal recorded)")
                 ""
                 (format "**Acceptance checks** (run by `steer done ~a`, which refuses on failure):" id)
                 (if (null? (task-ref t 'checks '()))
                     "- none yet (closing needs an explicit `--unverified` reason)"
                     (string-join (for/list ([c (task-ref t 'checks)]) (format "- `~a`" c)) "\n"))
                 (and (pair? (task-ref t 'after '()))
                      (string-append "\n**Depends on:** " (string-join (for/list ([d (task-ref t 'after)]) (ref-text by-id d)) ", ")))
                 (and (pair? (task-ref t 'anchors '()))
                      (string-append "\n**Code anchors:** " (string-join (for/list ([a (task-ref t 'anchors)]) (format "`~a`" (hash-ref a 'ref))) ", ")))
                 ""
                 (format "<sub>Mirrored from this repo's `.steer/` task store (the source of truth) by `steer github sync`. Details: `steer show ~a`.</sub>" id)
                 (format "<!-- steer:~a -->" id)))
   "\n"))

(define (render-milestone-description t)
  (string-append (or (task-ref t 'goal) "")
                 (if (null? (task-ref t 'checks '())) ""
                     (string-append "\n\nProof: " (string-join (map (λ (c) (format "`~a`" c)) (task-ref t 'checks)) "; ")))
                 (format "\n\nsteer task ~a" (task-ref t 'id))))

(define (issue-desired t by-id milestone-id)
  (define st (cond [(eq? (task-ref t 'status) 'done) "completed"] [(eq? (task-ref t 'status) 'dropped) "not_planned"] [else #f]))
  (hasheq 'title (format "~a · ~a" (task-ref t 'id) (task-ref t 'title))
          'body (render-issue-body t by-id)
          'labels (filter (λ (l) (not (equal? l "milestone"))) (tags-of t))
          'milestone milestone-id
          'state (if st "closed" "open")
          'state_reason st))

(define (milestone-desired t)
  (hasheq 'title (task-ref t 'title) 'description (render-milestone-description t) 'state (if (closed? t) "closed" "open")))

(define (desired-digest d) (digest (for/list ([k (sort (hash-keys d) symbol<?)]) (cons k (hash-ref d k)))))

;; ---------------------------------------------------------------------------------------------
;; Planning: → list of actions (kind task-id desired), in execution order: milestones, then issues
;; (dependencies before dependents, so bodies can link issue numbers where they already exist).

(define (sync-plan selected by-id)
  (define ms (filter milestone-task? selected))
  (define issues (filter (λ (t) (not (milestone-task? t))) selected))
  (define (milestone-of t)
    (for/first ([m ms] #:when (member (task-ref t 'id) (task-ref m 'after '()))) (task-ref m 'id)))
  (define (action kind t d)
    (define dg (desired-digest d))
    (cond [(not (gh-field t 'number)) (list 'create kind (task-ref t 'id) d dg)]
          [(not (equal? (gh-field t 'digest) dg)) (list 'update kind (task-ref t 'id) d dg)]
          [else #f]))
  (define ordered-issues
    (let ([rank (for/hash ([l (layers (for/hash ([t issues]) (values (task-ref t 'id) (hash-set t 'status 'open))))]
                           [i (in-naturals)] #:when #t [id l])
                  (values id i))])
      (sort issues (λ (a b) (define ra (hash-ref rank (task-ref a 'id) 0)) (define rb (hash-ref rank (task-ref b 'id) 0))
                     (or (< ra rb) (and (= ra rb) (< (id-number (task-ref a 'id)) (id-number (task-ref b 'id)))))))))
  (filter values
          (append (for/list ([m ms]) (action 'milestone m (milestone-desired m)))
                  (for/list ([t ordered-issues]) (action 'issue t (issue-desired t by-id (milestone-of t)))))))

;; ---------------------------------------------------------------------------------------------
;; gh

(define (gh-path)
  (or (getenv "STEER_GH")
      (let ([p (find-executable-path "gh")]) (and p (path->string p)))
      (fail! 'no-gh "`gh` (GitHub CLI) not found on PATH" #:hint "install gh and run `gh auth login`" #:code 3)))

;; → (values exit-code stdout-string stderr-string)
(define (gh . args)
  (define-values (p out in err) (apply subprocess #f #f #f (gh-path) args))
  (close-output-port in)
  (define err-text (box ""))
  (define t (thread (λ () (set-box! err-text (port->string err)))))
  (define out-text (port->string out))
  (subprocess-wait p)
  (thread-wait t)
  (close-input-port out) (close-input-port err)
  (values (subprocess-status p) out-text (unbox err-text)))

(define (gh-json! what . args)
  (define-values (code out err) (apply gh args))
  (unless (eqv? code 0)
    (fail! 'github (format "gh failed while ~a: ~a" what (clip (one-line (string-append out " " err)) 300))
           #:hint "check `gh auth status` and the repo name; nothing after this point was sent" #:code 3))
  (with-handlers ([exn:fail? (λ (e) (hasheq))]) (string->jsexpr out)))

(define (detect-repo root)
  (or (config-ref root 'github-repo #f)
      (let* ([o (with-output-to-string (λ () (parameterize ([current-directory root] [current-error-port (open-output-nowhere)])
                                              (system* (find-executable-path "git") "remote" "get-url" "origin"))))]
             [m (regexp-match #px"github\\.com[:/]([^/]+)/([^/\\s]+?)(?:\\.git)?\\s*$" o)])
        (and m (string-append (cadr m) "/" (caddr m))))
      (fail! 'no-repo "cannot tell which GitHub repo to use" #:hint "pass --repo OWNER/NAME")))

(define (ensure-labels! root repo labels)
  (define known (config-ref root 'github-labels '()))
  (define missing (filter (λ (l) (not (member l known))) (remove-duplicates labels)))
  (for ([l missing])
    (define-values (code out err) (gh "api" (format "repos/~a/labels" repo) "-f" (string-append "name=" l) "-f" "color=c5def5"))
    (unless (or (eqv? code 0) (regexp-match? #rx"already_exists" (string-append out err)))
      (fail! 'github (format "could not create label ~a: ~a" l (clip (one-line err) 200)) #:code 3)))
  (unless (null? missing)
    (save-config! root (hash-set (load-config root) 'github-labels (append known missing)))))

(define (execute! root repo a by-id-box)
  (define-values (op kind id d dg) (apply values a))
  (define t (hash-ref (unbox by-id-box) id))
  (define number (gh-field t 'number))
  (define (fields)
    (case kind
      [(milestone) (list "-f" (string-append "title=" (hash-ref d 'title))
                         "-f" (string-append "description=" (hash-ref d 'description))
                         "-f" (string-append "state=" (hash-ref d 'state)))]
      [(issue)
       (define mt (and (hash-ref d 'milestone) (hash-ref (unbox by-id-box) (hash-ref d 'milestone) #f)))
       (define mnum (and mt (gh-field mt 'number)))
       (append (list "-f" (string-append "title=" (hash-ref d 'title)) "-f" (string-append "body=" (hash-ref d 'body)))
               (if mnum (list "-F" (format "milestone=~a" mnum)) '())
               (append* (for/list ([l (hash-ref d 'labels)]) (list "-f" (string-append "labels[]=" l))))
               (if (equal? (hash-ref d 'state) "closed")
                   (list "-f" "state=closed" "-f" (string-append "state_reason=" (hash-ref d 'state_reason)))
                   (if (eq? op 'update) (list "-f" "state=open") '())))]))
  (define path (format "repos/~a/~a" repo (if (eq? kind 'milestone) "milestones" "issues")))
  (define j
    (case op
      [(create) (apply gh-json! (format "creating ~a for ~a" kind id) "api" path (fields))]
      [(update) (apply gh-json! (format "updating ~a for ~a" kind id) "api" "-X" "PATCH" (format "~a/~a" path number) (fields))]))
  ;; a new issue is created open; close it in a second call if the task is already finished
  (when (and (eq? op 'create) (eq? kind 'issue) (equal? (hash-ref d 'state) "closed"))
    (gh-json! (format "closing issue for ~a" id) "api" "-X" "PATCH" (format "~a/~a" path (hash-ref j 'number))
              "-f" "state=closed" "-f" (string-append "state_reason=" (hash-ref d 'state_reason))))
  (define n (or number (hash-ref j 'number #f)))
  (unless n (fail! 'github (format "GitHub returned no number for ~a" id) #:code 3))
  (define t* (task-set t 'github (hasheq 'kind kind 'number n 'digest dg)))
  (save-task! root t*)
  (append-event! root 'github id (format "~a ~a #~a" op kind n))
  (set-box! by-id-box (hash-set (unbox by-id-box) id t*))
  (list op kind id n))

;; ---------------------------------------------------------------------------------------------
;; CLI

(define (cmd-github argv)
  (define-values (pos o) (parse-args "github" argv '(("--tag" many) ("--all" bool) ("--dry-run" bool) ("--repo" one))
                                     #:min 1 #:max 1
                                     #:usage "steer github sync (--tag T... | --all) [--dry-run] [--repo OWNER/NAME]"))
  (unless (equal? (car pos) "sync") (fail! 'usage (format "unknown github action ~a" (car pos)) #:hint "sync"))
  (define tags (opt-list o 'tag))
  (when (and (null? tags) (not (opt-ref o 'all)))
    (fail! 'usage "choose what to publish: --tag TAG (repeatable) or --all" #:hint "issues are public on a public repo"))
  (define root (find-root))
  (define repo (or (opt-ref o 'repo) (detect-repo root)))
  (with-store-lock root
    (λ ()
      (define tasks (load-tasks root))
      (define by-id (index tasks))
      (define selected (filter (λ (t) (or (opt-ref o 'all) (for/or ([g tags]) (member g (tags-of t))))) tasks))
      (define plan (sync-plan selected by-id))
      (define (action-line a) (format "~a ~a ~a: ~a" (car a) (cadr a) (caddr a) (hash-ref (cadddr a) 'title)))
      (cond
        [(opt-ref o 'dry-run)
         (make-reply "github" (string-join (cons (format "dry run for ~a: ~a action~a over ~a selected task~a"
                                                         repo (length plan) (plural (length plan)) (length selected) (plural (length selected)))
                                                 (map action-line plan))
                                           "\n")
                     (hasheq 'repo repo 'actions (length plan)))]
        [else
         (ensure-labels! root repo (append* (for/list ([a plan] #:when (eq? (cadr a) 'issue)) (hash-ref (cadddr a) 'labels))))
         (define box* (box by-id))
         (define done (for/list ([a plan]) (execute! root repo a box*)))
         (define (count op kind) (length (filter (λ (r) (and (eq? (car r) op) (eq? (cadr r) kind))) done)))
         (make-reply "github"
                     (string-join
                      (cons (format "synced ~a selected task~a to ~a: milestones ~a created ~a updated · issues ~a created ~a updated"
                                    (length selected) (plural (length selected)) repo
                                    (count 'create 'milestone) (count 'update 'milestone) (count 'create 'issue) (count 'update 'issue))
                            (for/list ([r done]) (format "~a ~a ~a → #~a" (car r) (cadr r) (caddr r) (cadddr r))))
                      "\n")
                     (hasheq 'repo repo 'results (for/list ([r done]) (hasheq 'op (car r) 'kind (cadr r) 'task (caddr r) 'number (cadddr r)))))]))))
