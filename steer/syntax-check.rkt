#lang racket/base
;; Structural validator for Racket source (catalog A1). The reader gives the authoritative error;
;; a bracket scanner adds the *likely* location, because "expected `)` to close `(` at line 10"
;; points at the opener, while the missing paren is usually where the next top-level form begins.
(require racket/list racket/string "common.rkt" "srcread.rkt")
(provide check-source)

;; → (values findings form-count lang)
(define (check-source text file)
  (define-values (forms lang read-finding)
    (with-handlers ([exn:fail:read?
                     (λ (e)
                       (define loc (let ([ls (exn:fail:read-srclocs e)]) (and (pair? ls) (car ls))))
                       (values #f #f
                               (finding 'error 'read-error (strip-location (exn-message e)) #:file file
                                        #:line (and loc (srcloc-line loc))
                                        #:col (and loc (srcloc-column loc) (add1 (srcloc-column loc))))))])
      (define-values (fs lang _t) (read-racket-source text #:source file))
      (values fs lang #f)))
  (define lang* (or lang (lang-of text)))
  (define lang-findings
    (cond [(not (regexp-match? #rx"\\.rkt$" file)) '()]
          [(not lang*) (list (finding 'warning 'no-lang "no #lang line; a .rkt module usually starts with `#lang racket/base`" #:file file #:line 1))]
          [(not (sexp-lang? lang*)) (list (finding 'info 'not-sexp (format "#lang ~a is not plain s-expressions; structural check skipped" lang*) #:file file #:line 1))]
          [else '()]))
  (cond
    [(and lang* (not (sexp-lang? lang*))) (values lang-findings 0 lang*)]
    [read-finding (values (append (list read-finding) (scan-findings text file) lang-findings) 0 lang*)]
    [else (values lang-findings (length forms) lang*)]))

(define (lang-of text)
  (define m (regexp-match #px"(?m:^#lang[ \t]+([^\\s]+))" text))
  (and m (cadr m)))

(define (strip-location msg)
  (car (string-split (regexp-replace #px"^[^:]*:[0-9]+:[0-9]+: " msg "") "\n")))

;; ---------------------------------------------------------------------------------------------
;; Bracket scanner: aware of strings, ; and #| |# comments, #\ char literals and |quoted| symbols.

(define openers (hash #\( #\) #\[ #\] #\{ #\}))
(define closers (hash #\) #\( #\] #\[ #\} #\{))

(define (scan-findings text file)
  (define n (string-length text))
  (define found '())
  (define (report! f) (set! found (cons f found)))
  (let loop ([i 0] [line 1] [col 0] [stack '()] [last-code-line 0])
    (cond
      [(>= i n)
       (when (pair? stack)
         (define outer (last stack))
         (report! (finding 'error 'unclosed
                             (format "~a unclosed bracket~a at end of file; the outermost is `~a` at line ~a"
                                     (length stack) (plural (length stack)) (car outer) (cadr outer))
                             #:file file #:line (cadr outer) #:col (add1 (caddr outer))
                             #:fix (format "add ~a at the end of the form that starts on line ~a"
                                           (apply string (map (λ (s) (hash-ref openers (car s))) stack))
                                           (cadr outer)))))]
      [else
       (define c (string-ref text i))
       (cond
         ;; line comment
         [(char=? c #\;) (let skip ([j i]) (if (or (>= j n) (char=? (string-ref text j) #\newline))
                                               (loop j line (+ col (- j i)) stack last-code-line)
                                               (skip (add1 j))))]
         ;; block comment #| ... |# (nestable)
         [(and (char=? c #\#) (< (add1 i) n) (char=? (string-ref text (add1 i)) #\|))
          (let skip ([j (+ i 2)] [depth 1] [ln line] [cl (+ col 2)])
            (cond [(>= j n) (loop j ln cl stack last-code-line)]
                  [(and (char=? (string-ref text j) #\|) (< (add1 j) n) (char=? (string-ref text (add1 j)) #\#))
                   (if (= depth 1) (loop (+ j 2) ln (+ cl 2) stack last-code-line) (skip (+ j 2) (sub1 depth) ln (+ cl 2)))]
                  [(and (char=? (string-ref text j) #\#) (< (add1 j) n) (char=? (string-ref text (add1 j)) #\|))
                   (skip (+ j 2) (add1 depth) ln (+ cl 2))]
                  [(char=? (string-ref text j) #\newline) (skip (add1 j) depth (add1 ln) 0)]
                  [else (skip (add1 j) depth ln (add1 cl))]))]
         ;; here string #<<NAME ... NAME
         [(and (char=? c #\#) (regexp-match #px"^#<<([^\n]*)\n" text i))
          => (λ (m)
               (define term (string-append "\n" (cadr m) "\n"))
               (define end (let ([p (regexp-match-positions (regexp-quote term) text i)]) (if p (cdar p) n)))
               (define lines (for/sum ([ch (in-string text i end)]) (if (char=? ch #\newline) 1 0)))
               (loop end (+ line lines) 0 stack (+ line lines -1)))]
         ;; char literal #\x, #\space, #\(
         [(and (char=? c #\#) (< (add1 i) n) (char=? (string-ref text (add1 i)) #\\))
          (define j (let skip ([j (min n (+ i 3))])
                      (if (and (< j n) (char-alphabetic? (string-ref text j))) (skip (add1 j)) j)))
          (loop j line (+ col (- j i)) stack line)]
         ;; strings (also #"..", #rx"..", #px"..")
         [(char=? c #\")
          (let skip ([j (add1 i)] [ln line] [cl (add1 col)])
            (cond [(>= j n) (loop j ln cl stack ln)]
                  [(char=? (string-ref text j) #\\) (skip (min n (+ j 2)) ln (+ cl 2))]
                  [(char=? (string-ref text j) #\") (loop (add1 j) ln (add1 cl) stack ln)]
                  [(char=? (string-ref text j) #\newline) (skip (add1 j) (add1 ln) 0)]
                  [else (skip (add1 j) ln (add1 cl))]))]
         ;; |quoted symbol|
         [(char=? c #\|)
          (let skip ([j (add1 i)] [cl (add1 col)])
            (cond [(or (>= j n) (char=? (string-ref text j) #\newline)) (loop j line cl stack line)]
                  [(char=? (string-ref text j) #\|) (loop (add1 j) line (add1 cl) stack line)]
                  [else (skip (add1 j) (add1 cl))]))]
         [(hash-ref openers c #f)
          ;; A form opening in column 0 while brackets are still open: the previous top-level
          ;; form almost certainly lacks closers. Report that once; it is usually the root cause.
          (define reset? (and (= col 0) (pair? stack)))
          (when reset?
            (define outer (last stack))
            (report! (finding 'error 'unclosed-form
                              (format "the form starting at line ~a is still open (~a missing closer~a) when a new top-level form starts at line ~a"
                                      (cadr outer) (length stack) (plural (length stack)) line)
                              #:file file #:line (max (cadr outer) last-code-line)
                              #:fix (format "add ~a at the end of line ~a"
                                            (apply string (map (λ (s) (hash-ref openers (car s))) stack))
                                            last-code-line))))
          ;; after reporting, assume the old form ended and keep scanning for further problems
          (loop (add1 i) line (add1 col) (cons (list c line col) (if reset? '() stack)) line)]
         [(hash-ref closers c #f)
          => (λ (want)
               (cond
                 [(null? stack)
                  (report! (finding 'error 'extra-closer (format "`~a` at line ~a closes nothing" c line)
                                    #:file file #:line line #:col (add1 col) #:fix "delete it, or look for a missing opener before it"))
                  (loop (add1 i) line (add1 col) stack line)]
                 [(not (char=? (car (car stack)) want))
                  (define top (car stack))
                  (report! (finding 'error 'mismatched-closer
                                    (format "`~a` at line ~a col ~a closes `~a` from line ~a col ~a" c line (add1 col)
                                            (car top) (cadr top) (add1 (caddr top)))
                                    #:file file #:line line #:col (add1 col)
                                    #:fix (format "use `~a` here, or fix the opener" (hash-ref openers (car top)))))
                  (loop (add1 i) line (add1 col) (cdr stack) line)]
                 [else (loop (add1 i) line (add1 col) (cdr stack) line)]))]
         [(char=? c #\newline) (loop (add1 i) (add1 line) 0 stack last-code-line)]
         [(char-whitespace? c) (loop (add1 i) line (add1 col) stack last-code-line)]
         [else (loop (add1 i) line (add1 col) stack line)])]))
  (reverse found))
