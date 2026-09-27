#lang racket/base
;; `steer spec check|render FILE|-`: the CLI over spec.rkt (catalog G2, milestone M1).
;;   steer spec check FILE|-  [--lax]   parse and lint criteria, one per line; exit 1 on refusals
;;   steer spec render FILE|-           print the canonical sentence for each criterion that parses
;; `-` reads standard input, so an agent can pipe a heredoc. Lines starting with `#` and blank lines are
;; skipped. Findings are located (file:line:col) with a fix; the finding cap and --json follow the shared
;; protocol (note 03). Exit codes: 0 ok, 1 findings that refuse, 2 usage.
(require racket/list racket/string racket/port racket/file
         "common.rkt" "spec.rkt")
(provide cmd-spec)

(define (read-source src)
  (cond [(equal? src "-") (values (port->string (current-input-port)) "<stdin>")]
        [(file-exists? src) (values (file->string src) src)]
        [else (fail! 'usage (format "no such file ~a" src) #:hint "pass a file, or `-` to read standard input")]))

(define (by-position fs)
  (sort fs (λ (a b) (define la (hash-ref a 'line 0)) (define lb (hash-ref b 'line 0))
             (or (< la lb) (and (= la lb) (< (hash-ref a 'col 0) (hash-ref b 'col 0)))))))

(define (error? f) (eq? (hash-ref f 'severity) 'error))

;; one compact structured line per criterion (the full data is in --json)
(define (criterion-line c)
  (format "L~a ~a: ~a → ~a~a~a" (hash-ref c 'line) (hash-ref c 'shape) (hash-ref c 'system) (hash-ref c 'verb)
          (let ([o (hash-ref c 'object)])
            (cond [(string=? o "") ""] [(regexp-match? #rx"^," o) o] [else (string-append " " o)]))
          (let ([cond* (hash-ref c 'condition)]) (if cond* (format "  (~a: ~a)" (case (hash-ref c 'shape) [(event) "when"] [(state) "while"] [(unwanted) "if"] [else "where"]) cond*) ""))))

(define (cmd-spec argv)
  (define-values (pos o) (parse-args "spec" argv '(("--lax" bool)) #:min 2 #:max 2
                                     #:usage "steer spec check|render FILE|-  [--lax]"))
  (define action (car pos))
  (unless (member action '("check" "render"))
    (fail! 'usage (format "unknown spec action ~a" action) #:hint "check | render  (e.g. `steer spec check goal.txt`, or `steer spec check -` with a heredoc)"))
  (define-values (text name) (read-source (cadr pos)))
  (define-values (crits parse-findings) (parse-criteria text #:source name))
  (case action
    [("check")
     (define lint (lint-criteria crits #:source name #:lax? (opt-ref o 'lax)))
     (define none (if (and (null? crits) (null? parse-findings))
                      (list (finding 'error 'no-criteria (format "no criteria in ~a" name) #:file name
                                     #:fix (string-append "write one per line, e.g. `The export SHALL write a header row.`; forms: " templates-text)))
                      '()))
     (define findings (by-position (append none parse-findings lint)))
     (define bad-lines (remove-duplicates (for/list ([f findings] #:when (and (error? f) (hash-ref f 'line #f))) (hash-ref f 'line))))
     (define total (+ (length crits) (length (filter (λ (f) (memq (hash-ref f 'severity) '(error))) parse-findings))))
     (define refused (length (remove-duplicates (map (λ (f) (hash-ref f 'line)) (filter (λ (f) (and (error? f) (hash-ref f 'line #f))) findings)))))
     (define n-err (length (filter error? findings)))
     (define n-warn (- (length findings) n-err))
     (define good (filter (λ (c) (not (member (hash-ref c 'line) bad-lines))) crits))
     (make-reply "spec"
                 (string-join
                  (cons (format "~a criteri~a: ~a ok, ~a refused (~a error~a, ~a warning~a)"
                                (max total (length crits)) (if (= (max total (length crits)) 1) "on" "a") (length good) refused
                                n-err (plural n-err) n-warn (plural n-warn))
                        (map criterion-line good))
                  "\n")
                 (hasheq 'criteria (criteria->jsexpr crits) 'ok (length good) 'refused refused 'errors n-err 'warnings n-warn)
                 #:ok? (zero? n-err)
                 #:findings findings
                 #:next (if (zero? n-err) '() (list "fix the refused lines and rerun `steer spec check`")))]
    [else
     (make-reply "spec"
                 (render-criteria crits)
                 (hasheq 'criteria (criteria->jsexpr crits))
                 #:ok? (null? parse-findings)
                 #:findings (by-position parse-findings))]))
