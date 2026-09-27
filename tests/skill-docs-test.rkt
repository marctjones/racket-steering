#lang racket/base
;; T52: the skill, help text and README state per language what runs, what is exact and what is
;; skipped, so an agent on a Python or C# project neither over-trusts nor ignores the tools
;; (note 12's complaint: the docs used to only ever say "Racket").
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
;; the skill: states coverage before the how-to, and does not claim Racket-only capabilities (dup,
;; api) work for Python/C#

(define skill (file->string skill-path))
(check-regexp-match #rx"Racket, Python and C#" skill)
(check-regexp-match #rx"\\| language \\| syntax check \\| anchors" skill "a coverage table up front")
(for ([lang '("Racket (.rkt)" "Python (.py/.pyi)" "C# (.cs)")])
  (check-true (string-contains? skill lang) (format "skill table lists ~a" lang)))
(check-regexp-match #rx"anything else \\| skipped" skill)
(check-regexp-match #rx"Duplicates \\(Racket only\\)" skill "dup is not claimed to work on Python/C#")
(check-regexp-match #rx"Architecture rules \\(Racket only\\)" skill)
(check-regexp-match #rx"Public API drift \\(Racket only\\)" skill)
(check-regexp-match #rx"anchor-unresolved" skill "the did-you-mean distinction from T51 is documented here")
(check-regexp-match #rx"anchor-pending" skill)
(check-regexp-match #rx"pytest.*dotnet test.*raco test" skill "the three recognised test runners are named together")
(check-regexp-match #rx"~heuristic" skill)

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
