#lang racket/base
;; T52: the skill, help text and README state per language what runs, what is exact and what is
;; skipped, so an agent on a Python or C# project neither over-trusts nor ignores the tools
;; (note 12's complaint: the docs used to only ever say "Racket").
;; T69 extends the same pattern to the code graph (`rules`/`api --entries`): `steer help rules|api`
;; and the skill state, from one shared table, what is exact/declared/name-matched/invisible per
;; language, how to declare an entry, and that a dead finding is a may-call over-approximation -
;; `rules` and `api` are no longer described as Racket-only (dup still is).
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path racket/path)

(define-runtime-path main-rkt "../steer/main.rkt")
(define-runtime-path skill-path "../skills/steer-code/SKILL.md")
(define-runtime-path readme-path "../README.md")

(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))

;; ---------------------------------------------------------------------------------------------
;; `steer help syntax|stale|done` each state, for every one of these three languages, what steer
;; does — not just "Racket" (which is what note 12's own review found missing)

(for ([cmd '("syntax" "stale" "done")])
  (let-values ([(c o) (steer "help" cmd)])
    (check-equal? c 0 cmd)
    (for ([lang '("Racket" "Python" "C#")])
      (check-true (string-contains? o lang) (format "steer help ~a mentions ~a" cmd lang)))
    ;; a file type with no gate must never read as silently fine
    (check-regexp-match #rx"skipped" o (format "steer help ~a says what happens to an unlisted language" cmd))))

;; the three help texts agree with each other (one shared table, not three drifting descriptions)
(let-values ([(c1 o1) (steer "help" "syntax")] [(c2 o2) (steer "help" "stale")] [(c3 o3) (steer "help" "done")])
  (define (table-of o) (car (regexp-match (pregexp (string-append "\\| language \\|(?s:.*?)" "\n\n")) o)))
  (check-equal? (table-of o1) (table-of o2) "syntax and stale cite the same coverage table")
  (check-equal? (table-of o1) (table-of o3) "syntax and done cite the same coverage table"))

;; the table itself says what is exact and what needs an external tool
(let-values ([(c o) (steer "help" "syntax")])
  (check-regexp-match #rx"exact.*reader" o)
  (check-regexp-match #rx"exact.*compile\\(\\)" o "Python is named as exact, and the mechanism (compile) is named")
  (check-regexp-match #rx"no dotnet needed" o "C# does not require dotnet for this")
  (check-regexp-match #rx"needs python3" o "the Python dependency is stated"))

;; ---------------------------------------------------------------------------------------------
;; T69: `steer help rules|api` cite their own shared graph-coverage table - exact/declared/name-match/
;; invisible per language, how to declare an entry, and that `dead` is a may-call over-approximation.
;; `rules`/`api` are no longer described as Racket-only anywhere (dup still is - checked below).

(for ([cmd '("rules" "api")])
  (let-values ([(c o) (steer "help" cmd)])
    (check-equal? c 0 cmd)
    (for ([lang '("Racket" "Python" "C#")])
      (check-true (string-contains? o lang) (format "steer help ~a mentions ~a" cmd lang)))
    (check-regexp-match #rx"declared" o (format "steer help ~a names the declared confidence tag" cmd))
    (check-regexp-match #rx"name-match" o (format "steer help ~a names the name-match confidence tag" cmd))
    (check-regexp-match #rx"invisible" o (format "steer help ~a names what this graph cannot see at all" cmd))
    (check-false (regexp-match? #px"Racket[- ]only" o) (format "steer help ~a is not described as Racket-only" cmd))))

;; the two commands cite the SAME graph-coverage table (one shared table, not two drifting ones)
(let-values ([(c1 o1) (steer "help" "rules")] [(c2 o2) (steer "help" "api")])
  (define (table-of o) (car (regexp-match (pregexp (string-append "\\| language \\| same-file calls(?s:.*?)" "\n\n")) o)))
  (check-equal? (table-of o1) (table-of o2) "rules and api cite the same graph-coverage table"))

;; entries: how to declare one, and the may-call framing for dead
(let-values ([(c o) (steer "help" "rules")])
  (check-regexp-match #px"steer:\\s*entry" o "how to mark a symbol as an entry is documented")
  (check-regexp-match #rx"entry\\(" o "the .steer/rules.dl override mechanism is named")
  (check-regexp-match #rx"no call was found" o "a dead finding is framed as a may-call over-approximation, not a claim of unused code"))

;; ---------------------------------------------------------------------------------------------
;; the skill: states coverage before the how-to, and does not claim Racket-only capabilities (dup,
;; api) work for Python/C#

(define skill (file->string skill-path))
(check-regexp-match #rx"Racket, Python and C#" skill)
(check-regexp-match #rx"\\| language \\| syntax check \\| anchors" skill "a coverage table up front")
(for ([lang '("Racket (.rkt)" "Python (.py/.pyi)" "C# (.cs)")])
  (check-true (string-contains? skill lang) (format "skill table lists ~a" lang)))
(check-regexp-match #rx"anything else \\| skipped" skill)
(check-regexp-match #rx"Duplicates \\(Racket only\\)" skill "dup is not claimed to work on Python/C#")
(check-regexp-match #rx"anchor-unresolved" skill "the did-you-mean distinction from T51 is documented here")
(check-regexp-match #rx"anchor-pending" skill)
(check-regexp-match #rx"pytest.*dotnet test.*raco test" skill "the three recognised test runners are named together")
(check-regexp-match #rx"~heuristic" skill)

;; T69: the skill's own graph-coverage table, and `rules`/`api` are no longer claimed Racket-only
(check-regexp-match #rx"\\| language \\| same-file calls \\| cross-file calls \\| invisible" skill "the skill has its own graph-coverage table")
(check-false (regexp-match? #px"Architecture rules \\(Racket only\\)|Public API drift \\(Racket only\\)" skill)
             "rules/api sections are no longer labelled Racket-only")
(check-regexp-match #rx"no call was found" skill "the may-call framing for dead findings is stated in the skill too")
(check-regexp-match #px"steer:\\s*entry" skill "how to declare an entry is in the skill")

;; the installed copy (`.claude/skills/`) matches the source of truth, so an agent actually reading
;; from the project sees the same text
(define installed-path (build-path (path-only skill-path) 'up 'up ".claude" "skills" "steer-code" "SKILL.md"))
(when (file-exists? installed-path)
  (check-equal? (file->string installed-path) skill "installed copy matches skills/steer-code/SKILL.md"))

;; ---------------------------------------------------------------------------------------------
;; README's known-limits section names all three languages and is not "Racket only" by omission

(define readme (file->string readme-path))
(define limits (car (regexp-match (pregexp (string-append "## Known limits(?s:.*?)(?=" "\n" "## |$)")) readme)))
(check-regexp-match #rx"Racket" limits)
(check-regexp-match #rx"Python" limits)
(check-regexp-match #rx"C#" limits)
(check-regexp-match #rx"skipped" limits)
(check-regexp-match #rx"heuristic" limits)
;; the tool table itself doesn't still say a bare "Racket code" for something now multi-language
(define tool-table (car (regexp-match (pregexp (string-append "\\| area \\| commands(?s:.*?)" "\n\n")) readme)))
(check-false (regexp-match? #rx"\\| Racket code \\|" tool-table) "the row was relabelled, not just the limits section")

;; T69: README's known-limits section states rules/api are no longer Racket-only, and dup still is
(check-regexp-match #rx"`dup` is Racket-only" limits "dup's own limit is still stated plainly")
(check-false (regexp-match? #rx"`rules` (is|are) Racket-only" limits) "rules is not described as Racket-only")
(check-false (regexp-match? #rx"^`api` (is|are) Racket-only" limits) "api is not described as flatly Racket-only")
(check-regexp-match #rx"T55/T56" limits "the Roslyn precision upgrades are named as optional, not required")
