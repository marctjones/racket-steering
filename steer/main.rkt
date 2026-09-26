#lang racket/base
;; steer: deterministic steering tools for coding agents, as one CLI.
;; Global flags may appear anywhere: --json --full --limit N --agent NAME --root DIR
;; Exit codes (stable, hooks depend on them): 0 ok · 1 findings/refused · 2 usage · 3 internal.
(require racket/list racket/string racket/port json
         "common.rkt" "store.rkt" "cmd-tasks.rkt" "cmd-code.rkt" "skills.rkt")
(provide main)

(define version "0.1.0")

;; name, procedure, usage, summary, optional details
(struct command (name proc usage summary details))

(define (cmd name proc usage summary [details #f]) (command name proc usage summary details))

(define plan-format #<<EOF
Plan format (a file or stdin; all tasks are validated before any is written):
  (task "Add CSV export"
    #:id csv                        ; local label for #:after in the same plan
    #:goal "What and why, one or two sentences"
    #:after (schema T3)             ; labels from this plan or existing ids
    #:check "raco test report/export-test.rkt"   ; repeatable; `done` runs these
    #:anchor "report/export.rkt#export-csv"      ; code the plan depends on
    #:touch "report/"  #:priority 1  #:tag export)
Example: steer import - <<'PLAN'
(task "Schema" #:id schema #:check "raco test schema-test.rkt")
(task "Export" #:after (schema) #:check "raco test export-test.rkt")
PLAN
EOF
  )

(define commands
  (list
   (cmd "init" cmd-init "steer init [--skills] [--force]"
        "create .steer/ in the current directory; --skills also installs the Claude Code skills into .claude/skills")
   (cmd "add" cmd-add "steer add \"TITLE\" [--goal S] [--after T1,T2] [--check CMD]... [--anchor path#name]... [--touch P]... [--priority 0-9] [--tag T]..."
        "add one task; --check commands are what `done` runs")
   (cmd "import" cmd-import "steer import PLAN-FILE|- [--dry-run]" "add a whole checked plan at once" plan-format)
   (cmd "list" cmd-list "steer list [--status open|ready|blocked|active|done|dropped|all] [--tag T]" "one line per task (default: unfinished work)")
   (cmd "show" cmd-show "steer show ID [--full]" "the full packet for one task: goal, deps, checks, anchors, recent log")
   (cmd "ready" cmd-ready "steer ready" "tasks whose dependencies are done, best first")
   (cmd "next" cmd-next "steer next [--claim]" "the single best task to work on (your active task if you have one)")
   (cmd "claim" cmd-claim "steer claim ID [--force]" "mark a task active for --agent (refuses blocked or someone else's)")
   (cmd "release" cmd-release "steer release ID" "give an active task back")
   (cmd "note" cmd-note "steer note ID TEXT" "record a decision or finding on a task")
   (cmd "checkpoint" cmd-checkpoint "steer checkpoint ID --did S --next S [--question Q]... [--release]"
        "save progress so a fresh context can continue; both --did and --next are required")
   (cmd "done" cmd-done "steer done ID [--unverified REASON]" "run the task's checks; mark done only if all pass")
   (cmd "verify" cmd-verify "steer verify ID" "run the task's checks without changing its status")
   (cmd "drop" cmd-drop "steer drop ID --reason S" "abandon a task, with the reason")
   (cmd "reopen" cmd-reopen "steer reopen ID" "reopen a done or dropped task")
   (cmd "edit" cmd-edit "steer edit ID [--title S] [--goal S] [--priority N] [--add-/--rm-after|check|anchor|touch|tag X]..." "change a task; refuses cycles")
   (cmd "resume" cmd-resume "steer resume" "start-of-session packet: your active task, what's ready, stale anchors, the event cursor")
   (cmd "since" cmd-since "steer since CURSOR" "what happened after an event cursor (from `resume`)")
   (cmd "graph" cmd-graph "steer graph" "check the dependency graph: cycles, missing/dropped deps, layers, critical path")
   (cmd "stale" cmd-stale "steer stale" "anchors on open tasks whose code changed since the plan was written")
   (cmd "refresh" cmd-refresh "steer refresh ID... | --all" "accept the current code as the new anchor baseline after reviewing")
   (cmd "syntax" cmd-syntax "steer syntax FILE... [--fix]" "Racket structural check: reader error plus a verified repair; --fix applies it")
   (cmd "dup" cmd-dup "steer dup [PATH...] [--min-size N] [--loose]" "Racket clone detection: same code modulo local renaming")
   (cmd "api" cmd-api "steer api snapshot|diff|show [MODULE.rkt...]" "public API lock for Racket modules: exports, arity, contracts; diff classifies breaks")
   (cmd "skills" (λ (a) (cmd-skills a)) "steer skills list|install [--user] [--force] [NAME...]" "install the bundled Claude Code skills")
   (cmd "hook" (λ (a) (cmd-hook a)) "steer hook session-start|post-edit|config" "Claude Code hook entry points; `config` prints the settings.json snippet")
   (cmd "help" (λ (a) (cmd-help a)) "steer help [COMMAND]" "this overview, or details for one command")))

(define (find-command name) (findf (λ (c) (equal? (command-name c) name)) commands))

;; ---------------------------------------------------------------------------------------------
;; help / skills / hook

(define (cmd-help argv)
  (define-values (pos _) (parse-args "help" argv '() #:max 1))
  (cond
    [(null? pos)
     (make-reply "help"
                 (string-append
                  (format "steer ~a — deterministic steering tools for coding agents\n" version)
                  "usage: steer COMMAND [ARGS] [--json] [--full] [--limit N] [--agent NAME] [--root DIR]\n"
                  (string-join (for/list ([c commands]) (format "  ~a~a ~a" (command-name c)
                                                                (make-string (max 1 (- 11 (string-length (command-name c)))) #\space)
                                                                (command-summary c)))
                               "\n")
                  "\n`steer help COMMAND` for usage. Exit codes: 0 ok, 1 findings/refused, 2 usage, 3 internal.\n"
                  "Agent identity: --agent NAME or STEER_AGENT (default \"claude\")."))]
    [else
     (define c (find-command (car pos)))
     (unless c (unknown-command (car pos)))
     (make-reply "help" (string-join (filter values (list (command-usage c) (command-summary c) (command-details c))) "\n"))]))

(define (cmd-skills argv)
  (define-values (pos o) (parse-args "skills" argv '(("--user" bool) ("--force" bool)) #:min 1
                                     #:usage "steer skills list | steer skills install [--user] [--force] [NAME...]"))
  (case (car pos)
    [("list")
     (make-reply "skills" (string-join (for/list ([n (skill-names)]) n) "\n") (hasheq 'skills (skill-names)))]
    [("install")
     (define dest (if (opt-ref o 'user)
                      (build-path (find-system-path 'home-dir) ".claude" "skills")
                      (build-path (or (find-root #:required? #f) (current-directory)) ".claude" "skills")))
     (define only (if (null? (cdr pos)) #f (cdr pos)))
     (when only
       (for ([n only]) (unless (member n (skill-names))
                         (fail! 'usage (format "no bundled skill ~a" n) #:hint (did-you-mean n (skill-names) #:else (string-join (skill-names) " "))))))
     (define rs (install-skills! dest #:force? (opt-ref o 'force) #:only only))
     (make-reply "skills" (string-join (for/list ([r rs]) (format "~a: ~a" (car r) (cdr r))) "\n")
                 (hasheq 'dest (path->string dest) 'results (for/list ([r rs]) (hasheq 'name (car r) 'status (cdr r)))))]
    [else (fail! 'usage (format "unknown skills action ~a" (car pos)) #:hint "list | install")]))

;; Hooks must never break the session: problems go to stderr and exit 0, except post-edit, which
;; exits 2 to show structural errors to Claude (Claude Code PostToolUse semantics).
(define (cmd-hook argv)
  (define-values (pos _) (parse-args "hook" argv '() #:min 1 #:max 1 #:usage "steer hook session-start|post-edit|config"))
  (case (car pos)
    [("session-start")
     (define root (find-root #:required? #f))
     (cond
       [root
        (define-values (text _d findings next) (resume-text root))
        (make-reply "hook" (string-join (filter (λ (s) (not (string=? s "")))
                                                (list text
                                                      (string-join (map finding->text (take findings (min 5 (length findings)))) "\n")
                                                      (if (null? next) "" (string-append "next: " (string-join next " | ")))
                                                      "(steer task tracker; use the steer-tasks skill)"))
                                        "\n"))]
       [else (make-reply "hook" "")])]
    [("post-edit")
     (define input (with-handlers ([exn:fail? (λ (e) #f)]) (read-json (current-input-port))))
     (define file (and (hash? input)
                       (let ([ti (hash-ref input 'tool_input #f)]) (and (hash? ti) (hash-ref ti 'file_path #f)))))
     (define problems (and (string? file) (post-edit-problems file)))
     (when problems                         ; exit 2: Claude Code shows stderr to the model
       (write-string problems (current-error-port))
       (newline (current-error-port))
       (flush-output (current-error-port))
       (exit 2))
     (make-reply "hook" "")]
    [("config")
     (make-reply "hook" hook-config (hasheq 'settings (string->jsexpr hook-config)))]
    [else (fail! 'usage (format "unknown hook ~a" (car pos)) #:hint "session-start | post-edit | config")]))

;; For .claude/settings.json (project) or ~/.claude/settings.json (user). SessionStart without a
;; matcher also fires after /clear and compaction, which is what makes "checkpoint, clear, resume"
;; work: the fresh context receives the resume packet automatically.
(define hook-config #<<JSON
{
  "hooks": {
    "SessionStart": [
      { "hooks": [ { "type": "command", "command": "steer hook session-start" } ] }
    ],
    "PostToolUse": [
      { "matcher": "Edit|Write|MultiEdit",
        "hooks": [ { "type": "command", "command": "steer hook post-edit" } ] }
    ]
  },
  "permissions": { "allow": [ "Bash(steer *)" ] }
}
JSON
  )

(define (unknown-command name)
  (fail! 'usage (format "unknown command ~a" name)
         #:hint (did-you-mean name (map command-name commands) #:else "steer help")))

;; ---------------------------------------------------------------------------------------------
;; Entry

;; Pull global flags out of argv wherever they appear.
(define (split-globals argv)
  (let loop ([as argv] [rest '()])
    (cond
      [(null? as) (reverse rest)]
      [(equal? (car as) "--") (append (reverse rest) as)]
      [(equal? (car as) "--json") (current-json? #t) (loop (cdr as) rest)]
      [(equal? (car as) "--full") (current-full? #t) (loop (cdr as) rest)]
      [(member (car as) '("--limit" "--agent" "--root"))
       (when (null? (cdr as)) (fail! 'usage (format "~a needs a value" (car as))))
       (define v (cadr as))
       (case (car as)
         [("--limit") (define n (string->number v))
                      (unless (exact-positive-integer? n) (fail! 'usage "--limit needs a positive integer"))
                      (current-limit n)]
         [("--agent") (current-agent v)]
         [("--root") (current-root-override v)])
       (loop (cddr as) rest)]
      [(regexp-match #rx"^--(limit|agent|root)=(.*)$" (car as))
       => (λ (m) (loop (list* (string-append "--" (cadr m)) (caddr m) (cdr as)) rest))]
      [else (loop (cdr as) (cons (car as) rest))])))

(define (emit r elapsed)
  (if (current-json?)
      (begin (write-json (reply->jsexpr r elapsed)) (newline))
      (let ([t (reply->text r)]) (unless (string=? t "") (displayln t)))))

(define (error-reply kind msg hint)
  (make-reply "steer" "" (hasheq 'error kind) #:ok? #f
              #:findings (list (finding 'error kind msg #:fix hint))))

(define (main . argv)
  (define start (current-inexact-milliseconds))
  (define (elapsed) (inexact->exact (round (- (current-inexact-milliseconds) start))))
  (when (getenv "STEER_AGENT") (current-agent (getenv "STEER_AGENT")))
  (define code
    (with-handlers ([exn:steer? (λ (e) (emit (error-reply (exn:steer-kind e) (exn-message e) (exn:steer-hint e)) (elapsed))
                                  (exn:steer-code e))]
                    [exn:break? (λ (e) 130)]
                    [exn:fail? (λ (e) (emit (error-reply 'internal (car (string-split (exn-message e) "\n"))
                                                         "this is a steer bug; rerun with PLT_STEER_DEBUG=1 for a trace")
                                            (elapsed))
                                 (when (getenv "PLT_STEER_DEBUG") ((error-display-handler) (exn-message e) e))
                                 3)])
      (define args (split-globals argv))
      (cond
        [(or (null? args) (member (car args) '("-h" "--help"))) (emit (cmd-help '()) (elapsed)) 0]
        [(member (car args) '("-v" "--version")) (displayln (string-append "steer " version)) 0]
        [else
         (define c (or (find-command (car args)) (unknown-command (car args))))
         (define rest (cdr args))
         (cond
           [(or (member "--help" rest) (member "-h" rest)) (emit (cmd-help (list (car args))) (elapsed)) 0]
           [else
            (define r ((command-proc c) rest))
            (emit r (elapsed))
            (if (reply-ok? r) 0 1)])])))
  (flush-output)
  (exit code))

(module+ main
  (apply main (vector->list (current-command-line-arguments))))
