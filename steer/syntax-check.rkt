#lang racket/base
;; Structural validator for Racket source (catalog A1). The reader gives the authoritative error;
;; a bracket scanner adds the *likely* location, because "expected `)` to close `(` at line 10"
;; points at the opener, while the missing paren is usually where the next top-level form begins.
(require racket/list racket/string "common.rkt" "srcread.rkt")
(provide check-source apply-edit code-end-col edit-text)

;; → (values findings form-count lang)
(define (check-source text file)
  (define lang0 (header-lang text))
  (if (sexp-lang? lang0)
      (check-sexp-source text file)
      (values (list (finding 'info 'not-sexp (format "~a is not plain s-expressions; structural check skipped"
                                                     (if (regexp-match? #rx"^#reader" lang0) lang0 (string-append "#lang " lang0)))
                             #:file file #:line 1))
              0 lang0)))

(define (check-sexp-source text file)
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
  (define lang* (or lang (header-lang text)))
  (define lang-findings
    (cond [(not (regexp-match? #rx"\\.rkt$" file)) '()]
          [(and (not lang*) (not (module-form? forms)))
           (list (finding 'warning 'no-lang "no #lang line; a .rkt module usually starts with `#lang racket/base`" #:file file #:line 1))]
          [else '()]))
  (cond
    [read-finding (values (append (list read-finding) (scan-findings text file) lang-findings) 0 lang*)]
    [else (values lang-findings (length forms) lang*)]))

;; `(module name lang body ...)` written out in full is a module too (common in Racket's own collects)
(define (module-form? forms)
  (and (pair? forms) (let ([d (syntax-e (car forms))]) (and (pair? d) (eq? (syntax-e (car d)) 'module)))))

(define (strip-location msg)
  (car (string-split (regexp-replace #px"^[^:]*:[0-9]+:[0-9]+: " msg "") "\n")))

;; ---------------------------------------------------------------------------------------------
;; Bracket diagnosis. The reader says *that* a file is broken and usually points at an opener; the
;; missing or extra bracket is often far away. Formatted Racket carries the missing information in
;; its indentation, so the diagnosis works in three steps:
;;   1. lex brackets and the first code column of every line (strings, comments, #\ chars,
;;      |symbols| and here strings skipped),
;;   2. propose repairs: (a) a line that starts at or left of a still-open opener's column means that
;;      opener should have closed at the end of the previous code line; (b) a line indented deeper
;;      than a form closed on the previous line means that close was premature; plus the reader's own
;;      trouble spots (a closer with nothing open, a mismatched closer, an open form at end of file),
;;   3. verify: apply each candidate, keep those after which the file reads, and choose the one that
;;      leaves the fewest indentation violations (earliest wins ties).
;; Measured on seeded errors over the corpus: scripts/syntax-accuracy.rkt, notes/10.

(define openers (hash #\( #\) #\[ #\] #\{ #\}))
(define closers (hash #\) #\( #\] #\[ #\} #\{))

(struct ev (kind ch line col idx) #:transparent)       ; kind: open | close | start

;; → (values events code-lines) where code-lines holds every line with code outside comments
(define (lex text)
  (define n (string-length text))
  (define evs '())
  (define code-lines (make-hasheqv))
  (define line 1)
  (define col 0)
  (define at-start? #t)
  (define (emit! k c i) (set! evs (cons (ev k c line col i) evs)))
  (define (code! c i)
    (hash-set! code-lines line #t)
    (when at-start?
      (set! at-start? #f)
      (emit! 'start c i)))
  (let loop ([i 0])
    (when (< i n)
      (define c (string-ref text i))
      (define (next i2 [dcol (- i2 i)]) (set! col (+ col dcol)) (loop i2))
      (define (skip-span j)                          ; consume text[i, j), tracking newlines
        (for ([ch (in-string text i j)])
          (if (char=? ch #\newline) (begin (set! line (add1 line)) (set! col 0) (set! at-start? #t)) (set! col (add1 col))))
        (loop j))
      (cond
        [(char=? c #\newline) (set! line (add1 line)) (set! col 0) (set! at-start? #t) (loop (add1 i))]
        [(char-whitespace? c) (next (add1 i))]
        [(char=? c #\;)
         (define j (let s ([j i]) (if (or (>= j n) (char=? (string-ref text j) #\newline)) j (s (add1 j)))))
         (next j)]
        [(and (char=? c #\#) (< (add1 i) n) (char=? (string-ref text (add1 i)) #\|))
         (define j (let s ([j (+ i 2)] [depth 1])
                     (cond [(>= j n) n]
                           [(and (char=? (string-ref text j) #\|) (< (add1 j) n) (char=? (string-ref text (add1 j)) #\#))
                            (if (= depth 1) (+ j 2) (s (+ j 2) (sub1 depth)))]
                           [(and (char=? (string-ref text j) #\#) (< (add1 j) n) (char=? (string-ref text (add1 j)) #\|))
                            (s (+ j 2) (add1 depth))]
                           [else (s (add1 j) depth)])))
         (skip-span j)]
        [(and (char=? c #\#) (regexp-match #px"^#<<([^\n]*)\n" text i))
         => (λ (m)
              (code! c i)
              (define term (string-append "\n" (cadr m) "\n"))
              (define end (let ([p (regexp-match-positions (regexp-quote term) text i)]) (if p (sub1 (cdar p)) n)))
              (skip-span end))]
        [(and (char=? c #\#) (< (add1 i) n) (char=? (string-ref text (add1 i)) #\\))
         (code! c i)
         (define j (let s ([j (min n (+ i 3))]) (if (and (< j n) (char-alphabetic? (string-ref text j))) (s (add1 j)) j)))
         (next j)]
        [(char=? c #\")
         (code! c i)
         (define j (let s ([j (add1 i)])
                     (cond [(>= j n) n]
                           [(char=? (string-ref text j) #\\) (s (min n (+ j 2)))]
                           [(char=? (string-ref text j) #\") (add1 j)]
                           [else (s (add1 j))])))
         (for ([ch (in-string text i j)])
           (if (char=? ch #\newline) (begin (set! line (add1 line)) (set! col 0)) (set! col (add1 col))))
         (hash-set! code-lines line #t)                ; the line where the string ends has code
         (loop j)]
        [(char=? c #\|)
         (code! c i)
         (define j (let s ([j (add1 i)]) (cond [(or (>= j n) (char=? (string-ref text j) #\newline)) j]
                                               [(char=? (string-ref text j) #\|) (add1 j)]
                                               [else (s (add1 j))])))
         (next j)]
        [(hash-ref openers c #f) (code! c i) (emit! 'open c i) (next (add1 i))]
        [(hash-ref closers c #f) (code! c i) (emit! 'close c i) (next (add1 i))]
        [else (code! c i) (next (add1 i))])))
  (values (reverse evs) code-lines))

(define (last-code-before code-lines line)
  (let loop ([l (sub1 line)]) (cond [(< l 1) 1] [(hash-ref code-lines l #f) l] [else (loop (sub1 l))])))

(define (closers-for opens) (apply string (map (λ (o) (hash-ref openers (ev-ch o))) opens)))

;; Walk the events. → (values violations candidates problems)
;; violations: count of indentation rules broken; candidates: (list kind edit message) in file order;
;; problems: the raw trouble spots (for the fallback message).
(define (analyze text evs code-lines)
  (define stack '())                                  ; open events, innermost first
  (define closes-on (make-hasheqv))                  ; line → list of (close . open)
  (define violations 0)
  (define cands '())
  (define problems '())
  (define last-line (let ([ls (hash-keys code-lines)]) (if (null? ls) 1 (apply max ls))))
  (define (cand! kind edit msg) (set! cands (cons (list kind edit msg) cands)))
  (for ([e evs])
    (case (ev-kind e)
      [(start)
       (unless (hash-ref closers (ev-ch e) #f)
         (define k (ev-col e))
         (define prev (last-code-before code-lines (ev-line e)))
         ;; (a) openers that should have closed before this line
         (define late (let take ([s stack]) (if (and (pair? s) (>= (ev-col (car s)) k)) (cons (car s) (take (cdr s))) '())))
         (when (pair? late)
           (set! violations (add1 violations))
           (define outer (last late))
           (cand! 'unclosed-form (insert-edit text prev (closers-for late))
                  (format "the form opened at line ~a col ~a is still open at line ~a, which is indented as if it had ended; missing ~a at the end of line ~a"
                          (ev-line outer) (add1 (ev-col outer)) (ev-line e) (closers-for late) prev)))
         ;; (b) a form closed on the previous line although this line is indented inside it
         (define early (filter (λ (co) (< (ev-col (cdr co)) k)) (hash-ref closes-on prev '())))
         (when (pair? early)
           (set! violations (add1 violations))
           (define co (car early))                   ; the last such close on that line
           (cand! 'extra-closer (hasheq 'op 'delete 'line (ev-line (car co)) 'col (add1 (ev-col (car co))) 'text (string (ev-ch (car co))))
                  (format "`~a` at line ~a col ~a closes the form from line ~a, but line ~a is still indented inside it"
                          (ev-ch (car co)) (ev-line (car co)) (add1 (ev-col (car co))) (ev-line (cdr co)) (ev-line e)))))]
      [(open) (set! stack (cons e stack))]
      [(close)
       (define want (hash-ref closers (ev-ch e)))
       (cond
         [(null? stack)
          (set! problems (cons (list 'extra e) problems))
          (cand! 'extra-closer (hasheq 'op 'delete 'line (ev-line e) 'col (add1 (ev-col e)) 'text (string (ev-ch e)))
                 (format "`~a` at line ~a col ~a closes nothing" (ev-ch e) (ev-line e) (add1 (ev-col e))))]
         [(not (char=? (ev-ch (car stack)) want))
          (define top (car stack))
          (set! problems (cons (list 'mismatch e top) problems))
          (define expected (string (hash-ref openers (ev-ch top))))
          (cand! 'mismatched-closer (hasheq 'op 'replace 'line (ev-line e) 'col (add1 (ev-col e)) 'text expected)
                 (format "`~a` at line ~a col ~a closes `~a` from line ~a col ~a"
                         (ev-ch e) (ev-line e) (add1 (ev-col e)) (ev-ch top) (ev-line top) (add1 (ev-col top))))
          (cand! 'unclosed-form (hasheq 'op 'insert 'line (ev-line e) 'col (add1 (ev-col e)) 'text expected)
                 (format "`~a` from line ~a col ~a is never closed; `~a` is missing before line ~a col ~a"
                         (ev-ch top) (ev-line top) (add1 (ev-col top)) expected (ev-line e) (add1 (ev-col e))))
          (set! stack (cdr stack))
          (hash-update! closes-on (ev-line e) (λ (l) (cons (cons e top) l)) '())]
         [else
          (hash-update! closes-on (ev-line e) (λ (l) (cons (cons e (car stack)) l)) '())
          (set! stack (cdr stack))])]))
  (when (pair? stack)
    (define outer (last stack))
    (set! problems (cons (list 'eof outer (length stack)) problems))
    (cand! 'unclosed-form (insert-edit text last-line (closers-for stack))
           (format "~a bracket~a still open at end of file; the outermost is `~a` at line ~a"
                   (length stack) (plural (length stack)) (ev-ch outer) (ev-line outer))))
  (values violations (reverse cands) (reverse problems)))

(define (readable? text) (with-handlers ([exn:fail? (λ (e) #f)]) (read-racket-source text) #t))

(define (violation-count text)
  (define-values (evs lines) (lex text))
  (define-values (v _c _p) (analyze text evs lines))
  v)

(define max-candidates 16)

(define (scan-findings text file)
  (define-values (evs lines) (lex text))
  (define-values (_v cands problems) (analyze text evs lines))
  (define unique
    (let loop ([cs cands] [seen (hash)] [acc '()])
      (cond [(or (null? cs) (>= (length acc) max-candidates)) (reverse acc)]
            [(hash-ref seen (cadr (car cs)) #f) (loop (cdr cs) seen acc)]
            [else (loop (cdr cs) (hash-set seen (cadr (car cs)) #t) (cons (car cs) acc))])))
  (define scored
    (for*/list ([c unique] [i (in-value (index-of unique c))]
                [fixed (in-value (with-handlers ([exn:fail? (λ (e) #f)]) (apply-edit text (cadr c))))]
                #:when (and fixed (readable? fixed)))
      (list (violation-count fixed) i c)))
  (define best (and (pair? scored) (caddr (argmin (λ (s) (+ (* 1000 (car s)) (cadr s))) scored))))
  (define (finding-of c verified?)
    (define e (cadr c))
    (finding 'error (car c) (string-append (caddr c) (if verified? " (verified: the file reads after this edit)" ""))
             #:file file #:line (hash-ref e 'line) #:col (hash-ref e 'col)
             #:fix (edit-text e) #:edit (hash-set e 'verified verified?)))
  (cond
    [best (list (finding-of best #t))]
    [(pair? unique) (list (finding-of (car unique) #f))]
    [else '()]))

(define (edit-text e)
  (case (hash-ref e 'op)
    [(insert) (format "insert ~a at line ~a col ~a" (hash-ref e 'text) (hash-ref e 'line) (hash-ref e 'col))]
    [(delete) (format "delete `~a` at line ~a col ~a" (hash-ref e 'text) (hash-ref e 'line) (hash-ref e 'col))]
    [(replace) (format "replace the closer at line ~a col ~a with `~a`" (hash-ref e 'line) (hash-ref e 'col) (hash-ref e 'text))]))

;; ---------------------------------------------------------------------------------------------
;; Edits: {op: insert|delete|replace, line, col (1-based), text}. Inserts at "end of line" go before
;; a trailing ; comment, otherwise the inserted closer would be commented out.

(define (insert-edit text line closers)
  (hasheq 'op 'insert 'line line 'col (code-end-col text line) 'text closers))

(define (line-start text line)                   ; 0-based index where `line` (1-based) begins
  (let loop ([i 0] [l 1])
    (cond [(= l line) i]
          [(>= i (string-length text)) (string-length text)]
          [(char=? (string-ref text i) #\newline) (loop (add1 i) (add1 l))]
          [else (loop (add1 i) l)])))

;; 1-based column just after the last code character on `line` (strings and #\; handled).
(define (code-end-col text line)
  (define start (line-start text line))
  (define end (let loop ([i start]) (if (or (>= i (string-length text)) (char=? (string-ref text i) #\newline)) i (loop (add1 i)))))
  (let loop ([i start] [last start])
    (cond
      [(>= i end) (add1 (- last start))]
      [else
       (define c (string-ref text i))
       (cond
         [(char=? c #\;) (add1 (- last start))]
         [(char=? c #\") (let skip ([j (add1 i)])
                           (cond [(>= j end) (add1 (- end start))]
                                 [(char=? (string-ref text j) #\\) (skip (+ j 2))]
                                 [(char=? (string-ref text j) #\") (loop (add1 j) (add1 j))]
                                 [else (skip (add1 j))]))]
         [(and (char=? c #\#) (< (add1 i) end) (char=? (string-ref text (add1 i)) #\\))
          (loop (min end (+ i 3)) (min end (+ i 3)))]
         [(char-whitespace? c) (loop (add1 i) last)]
         [else (loop (add1 i) (add1 i))])])))

(define (apply-edit text e)
  (define at (+ (line-start text (hash-ref e 'line)) (sub1 (hash-ref e 'col))))
  (case (hash-ref e 'op)
    [(insert) (string-append (substring text 0 at) (hash-ref e 'text) (substring text at))]
    [(delete) (string-append (substring text 0 at) (substring text (add1 at)))]
    [(replace) (string-append (substring text 0 at) (hash-ref e 'text) (substring text (add1 at)))]))
