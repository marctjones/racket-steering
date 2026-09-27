#lang racket/base
;; T28: the criteria grammar (parser-tools lex + yacc), its five EARS shapes, and located errors.
(require rackunit racket/list racket/string "../steer/spec.rkt")

(define (parse t #:line [line 1]) (parse-criterion t #:line line #:source "c.txt"))
(define (ok t) (let-values ([(c fs) (parse t)]) (check-equal? fs '() t) c))
(define (bad t) (let-values ([(c fs) (parse t)]) (check-false c t) (check-equal? (length fs) 1 (format "exactly one finding for: ~a" t)) (car fs)))
(define (fields c) (list (hash-ref c 'shape) (hash-ref c 'condition) (hash-ref c 'system) (hash-ref c 'verb) (hash-ref c 'object)))

;; ---------------------------------------------------------------------------------------------
;; the five shapes

(check-equal? (fields (ok "The export SHALL write a header row.")) '(ubiquitous #f "export" "write" "a header row"))
(check-equal? (fields (ok "WHEN the input is empty, the export SHALL write only the header row."))
              '(event "the input is empty" "export" "write" "only the header row"))
(check-equal? (fields (ok "WHILE the file is locked, the editor SHALL refuse edits."))
              '(state "the file is locked" "editor" "refuse" "edits"))
(check-equal? (fields (ok "IF the output file exists, THEN the export SHALL refuse and leave the file unchanged."))
              '(unwanted "the output file exists" "export" "refuse" "and leave the file unchanged"))
(check-equal? (fields (ok "WHERE force is set, the export SHALL overwrite the file."))
              '(optional "force is set" "export" "overwrite" "the file"))

;; ---------------------------------------------------------------------------------------------
;; what a criterion may contain

(check-equal? (fields (ok "the EXPORT shall WRITE nothing.")) '(ubiquitous #f "EXPORT" "write" "nothing") "keywords are case-insensitive; the verb is normalised")
(check-equal? (hash-ref (ok "The tool SHALL exit.") 'object) "" "a response may be just the verb")
(check-equal? (hash-ref (ok "The tool SHALL exit with code 2.") 'object) "with code 2")
(check-equal? (hash-ref (ok "The export SHALL return a list, sorted by name.") 'object) "a list, sorted by name" "commas are fine in the response")
(check-equal? (hash-ref (ok "The export of the report SHALL write x.") 'system) "export of the report" "`the` inside the system name")
(check-equal? (hash-ref (ok "WHEN the user writes a file, the tool SHALL log it.") 'condition) "the user writes a file" "verbs inside a condition are fine")
(check-equal? (hash-ref (ok "  The export SHALL write x.  ") 'text) "The export SHALL write x." "surrounding space is trimmed")
(check-equal? (hash-ref (ok "The export SHALL write x .") 'object) "x" "space before the full stop")
(let ([c (let-values ([(c fs) (parse "The export SHALL write x." #:line 7)]) c)])
  (check-equal? (hash-ref c 'line) 7))

;; clause keywords are ordinary words inside a phrase; they only start a clause
(check-equal? (hash-ref (ok "The tool SHALL keep the file if necessary.") 'object) "the file if necessary")
(check-equal? (hash-ref (ok "The tool SHALL write logs where possible.") 'object) "logs where possible")
(check-equal? (hash-ref (ok "WHEN the user is idle while the file is open, the tool SHALL exit.") 'condition) "the user is idle while the file is open")
(check-equal? (hash-ref (ok "The tool SHALL retry, then exit.") 'object) ", then exit" "a comma straight after the verb; `then` is a plain word here")
(check-equal? (hash-ref (ok "The tool SHALL retry twice, then exit.") 'object) "twice, then exit")
(check-equal? (hash-ref (bad "IF the file exists, the export SHALL refuse.") 'kind) 'missing-then "IF without THEN is still caught")

;; ---------------------------------------------------------------------------------------------
;; errors: kind, column, the token seen, and the template expected

(define (kind+col f) (list (hash-ref f 'kind) (hash-ref f 'col)))
(define (says f rx) (check-regexp-match rx (string-append (hash-ref f 'message) " → " (hash-ref f 'fix ""))))

(let ([f (bad "The export should write a header row.")])
  (check-equal? (kind+col f) '(weak-modal 12))
  (says f #rx"`should` is not allowed.*SHALL")
  (check-equal? (hash-ref f 'file) "c.txt")
  (check-equal? (hash-ref f 'line) 1))
(for ([w '("may" "might" "could" "would" "can" "must" "will")])
  (check-equal? (hash-ref (bad (format "The export ~a write x." w)) 'kind) 'weak-modal w))

(let ([f (bad "The code SHALL work.")])
  (check-equal? (kind+col f) '(no-observable 16))
  (says f #rx"after SHALL comes an observable verb \\(write, return.*found `work`")
  (says f #rx"SHALL return <value>"))
(let ([f (bad "The export SHALL writes a header row.")])
  (check-equal? (kind+col f) '(verb-form 18))
  (says f #rx"base form of the verb: `write`, not `writes`"))
(check-equal? (hash-ref (bad "The tool SHALL retries twice.") 'kind) 'verb-form "irregular plural (y → ies)")

(let ([f (bad "Exports are fast.")])
  (check-equal? (kind+col f) '(bad-start 1))
  (says f #rx"found `Exports`")
  (says f #rx"The <system> SHALL <verb> <what>\\.   WHEN <trigger>"))
(let ([f (bad "WHEN the input is empty the export SHALL write nothing.")])
  (check-equal? (hash-ref f 'kind) 'missing-comma)
  (says f #rx"comma after the WHEN condition"))
(check-equal? (hash-ref (bad "IF the file exists the export SHALL refuse.") 'kind) 'missing-comma)
(let ([f (bad "IF the file exists, the export SHALL refuse.")])
  (check-equal? (hash-ref f 'kind) 'missing-then)
  (says f #rx"expected THEN"))
(let ([f (bad "WHEN the file exists, export SHALL refuse.")])
  (check-equal? (hash-ref f 'kind) 'missing-the)
  (says f #rx"the <system>"))
(let ([f (bad "The export SHALL write a header row")])
  (check-equal? (kind+col f) '(missing-period 36))
  (says f #rx"add `\\.`"))
(check-equal? (hash-ref (bad "The export writes a header row.") 'kind) 'missing-shall)
(check-equal? (hash-ref (bad "") 'kind) 'empty-criterion)
(check-equal? (hash-ref (bad "   ") 'kind) 'empty-criterion)
(check-equal? (hash-ref (bad "SHALL write x.") 'kind) 'bad-start "SHALL with nothing before it")
(check-equal? (kind+col (bad "   Exports are fast.")) '(bad-start 4) "columns count leading spaces")

;; ---------------------------------------------------------------------------------------------
;; blocks: one criterion per line; comments and blank lines skipped; every error reported, in line order

(let-values ([(cs fs) (parse-criteria (string-join '("# acceptance criteria for CSV export"
                                                     ""
                                                     "The export SHALL write a header row."
                                                     "The export should write one row per record."
                                                     "WHEN the input is empty, the export SHALL write only the header row."
                                                     "Exports are fast.") "\r\n")
                                      #:source "goal.txt")])
  (check-equal? (map (λ (c) (hash-ref c 'line)) cs) '(3 5) "line numbers count skipped lines")
  (check-equal? (map (λ (f) (list (hash-ref f 'line) (hash-ref f 'kind))) fs) '((4 weak-modal) (6 bad-start)))
  (check-equal? (map (λ (f) (hash-ref f 'file)) fs) '("goal.txt" "goal.txt")))

(let-values ([(cs fs) (parse-criteria "")]) (check-equal? (list cs fs) '(() ())))
(let-values ([(cs fs) (parse-criteria "# only a comment\n\n")]) (check-equal? (list cs fs) '(() ())))

;; the data the messages are built from
(check-true (and (member "write" observable-verbs) (member "refuse" observable-verbs) (member "return" observable-verbs) #t))
(check-true (and (member "should" weak-modals) (not (member "shall" weak-modals)) #t))
(check-equal? shapes '(ubiquitous event state unwanted optional))
