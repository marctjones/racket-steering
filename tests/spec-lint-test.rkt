#lang racket/base
;; T29: lint rules that reject vague criteria that still parse, each with a located, actionable fix.
(require rackunit racket/list racket/string "../steer/spec.rkt")

(define (crit t) (let-values ([(c fs) (parse-criterion t #:line 4 #:source "g.txt")]) (check-equal? fs '() t) c))
(define (lint t #:lax? [lax? #f]) (lint-criterion (crit t) #:source "g.txt" #:lax? lax?))
(define (kinds t) (map (λ (f) (hash-ref f 'kind)) (lint t)))
(define (first-finding t) (car (lint t)))

;; ---------------------------------------------------------------------------------------------
;; every rule fires, with kind, severity, column, and a fix

(let ([f (first-finding "The export SHALL write CSV, JSON, etc.")])
  (check-equal? (list (hash-ref f 'kind) (hash-ref f 'severity) (hash-ref f 'col)) '(open-ended error 35))
  (check-equal? (list (hash-ref f 'file) (hash-ref f 'line)) '("g.txt" 4))
  (check-regexp-match #rx"open-ended list" (hash-ref f 'message))
  (check-regexp-match #rx"list every item" (hash-ref f 'fix)))
(check-equal? (kinds "The export SHALL write CSV and so on.") '(open-ended))
(check-equal? (kinds "The export SHALL write CSV and the like.") '(open-ended))

(let ([f (first-finding "The export SHALL write rows and/or a header.")])
  (check-equal? (list (hash-ref f 'kind) (hash-ref f 'severity) (hash-ref f 'col)) '(and-or error 29))
  (check-regexp-match #rx"split it into two criteria" (hash-ref f 'fix)))

(for ([t '("The tool SHALL write output quickly." "The tool SHALL write user-friendly output." "The tool SHALL write robust output."
           "The tool SHALL write reasonable defaults." "The tool SHALL return the best match." "The tool SHALL return properly encoded text."
           "The tool SHALL log errors gracefully.")])
  (check-equal? (kinds t) '(subjective) t))
(let ([f (first-finding "The tool SHALL return the best match.")])
  (check-regexp-match #rx"within 200 ms" (hash-ref f 'fix)))

(for ([t '("The tool SHALL retry as needed." "The tool SHALL write logs where possible." "The tool SHALL keep the file if necessary.")])
  (check-equal? (kinds t) '(hedge) t))

(for ([t '("The tool SHALL write several rows." "The tool SHALL retry a few times." "The tool SHALL return multiple results."
           "The tool SHALL return some rows." "The tool SHALL print a lot of lines.")])
  (check-equal? (kinds t) '(vague-quantity) t))

(let ([f (first-finding "The tool SHALL exit within 5.")])
  (check-equal? (list (hash-ref f 'kind) (hash-ref f 'severity) (hash-ref f 'col)) '(bare-quantity warning 28))
  (check-regexp-match #rx"add the unit" (hash-ref f 'fix)))
(check-equal? (kinds "WHEN idle for 5, the tool SHALL exit.") '(bare-quantity) "`for 5`")
(check-equal? (kinds "The tool SHALL return at most 10 and stop.") '(bare-quantity) "a number followed by a stop word")

(let ([f (first-finding "The export SHALL return the rows that are filtered.")])
  (check-equal? (list (hash-ref f 'kind) (hash-ref f 'severity)) '(passive warning))
  (check-regexp-match #rx"name the actor" (hash-ref f 'fix)))

(for ([t '("The tool SHALL exit with code 2, WHEN the input is malformed." "The tool SHALL exit with code 2 WHEN the input is malformed."
           "The tool SHALL exit with code 2, if the input is malformed." "The tool SHALL print the error, while the file is locked.")])
  (check-equal? (kinds t) '(misplaced-condition) t))
(let ([f (first-finding "The tool SHALL exit with code 2, WHEN the input is malformed.")])
  (check-equal? (hash-ref f 'severity) 'error)
  (check-regexp-match #rx"move it to the front: `WHEN <trigger>, the <system> SHALL" (hash-ref f 'fix)))
;; plain lower-case words are fine, and a leading condition is the right place
(for ([t '("The tool SHALL exit when done." "The tool SHALL keep the file where it is." "The tool SHALL retry, then exit."
           "WHEN the input is malformed, the tool SHALL exit with code 2.")])
  (check-equal? (kinds t) '() t))
(check-equal? (kinds "The tool SHALL keep the file if necessary.") '(hedge) "`if necessary` is a hedge, not a misplaced condition")

;; ---------------------------------------------------------------------------------------------
;; no false positives on well-formed criteria (the note 11 example and typical ones)

(for ([t '("The report module SHALL export records as CSV, one row per record."
           "The export SHALL write a header row taken from the record keys."
           "WHEN the output file exists and force is not set, the export SHALL refuse and leave the file unchanged."
           "WHEN the input is empty, the export SHALL write only the header row."
           "IF the input is malformed, THEN the tool SHALL exit with code 2 and print the line number."
           "WHILE the file is locked, the editor SHALL refuse edits."
           "WHERE force is set, the export SHALL overwrite the file."
           "The tool SHALL exit within 200ms."
           "The tool SHALL exit within 5 seconds."
           "The tool SHALL return at least 3 rows."
           "The tool SHALL return the file that is created by the importer."
           "The tool SHALL write the rows in the order given."
           "The tool SHALL exit with code 1."
           "The tool SHALL return sometimes-empty results.")])
  (check-equal? (kinds t) '() t))

;; ---------------------------------------------------------------------------------------------
;; scope and columns

(check-equal? (kinds "WHILE the file is locked, the editor SHALL refuse edits.") '() "a passive in the condition is a state, not a hidden actor")
(check-equal? (kinds "WHEN the input is empty, the tool SHALL write several rows.") '(vague-quantity))
(let ([fs (lint-criterion (crit "  The tool SHALL retry as needed, etc.") #:source "g.txt")])   ; two rules, leading spaces
  (check-equal? (map (λ (f) (hash-ref f 'kind)) fs) '(hedge open-ended) "sorted by column")
  (check-equal? (map (λ (f) (hash-ref f 'col)) fs) '(24 35) "columns index the original line, leading spaces included"))
(check-equal? (map (λ (f) (hash-ref f 'kind)) (lint "The tool SHALL retry as needed, etc.")) '(hedge open-ended) "several findings, one criterion")

;; --lax demotes refusals to warnings; rules that were warnings stay warnings
(check-equal? (map (λ (f) (hash-ref f 'severity)) (lint "The tool SHALL retry as needed." #:lax? #t)) '(warning))
(check-equal? (map (λ (f) (hash-ref f 'severity)) (lint "The tool SHALL retry as needed.")) '(error))

;; lint-criteria over a block, in line order
(let-values ([(cs fs) (parse-criteria "The tool SHALL retry as needed.\nThe tool SHALL return some rows.\nThe tool SHALL exit with code 1.\n")])
  (define out (lint-criteria cs #:source "g.txt"))
  (check-equal? (map (λ (f) (list (hash-ref f 'line) (hash-ref f 'kind))) out) '((1 hedge) (2 vague-quantity))))

;; the rules are data: every rule has an id, a message and a fix
(check-true (> (length lint-rules) 6))
