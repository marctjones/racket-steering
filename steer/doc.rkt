#lang racket/base
;; Documentation and signature lookup (catalog B1): real signatures instead of remembered ones.
;;   steer doc exists ID [MODULE]   is `ID` documented anywhere? which library? else nearest names
;;   steer doc sig ID [MODULE]      documented signature, argument contracts, description, (require ...)
;;   steer doc search WORD...       names containing every word
;;   steer doc exports MODULE       what a module provides: kind, arity, contract, documented?
;; Facts come from Racket's own documentation index (setup/xref) and the installed docs' HTML. A
;; standalone `raco exe` binary cannot see either, so a worker runs in the installed `racket`, the
;; same pattern as api.rkt. HTML parsing lives here, as pure functions, so it is unit-testable.
(require racket/list racket/string racket/file racket/port
         "common.rkt")
(provide cmd-doc parse-doc-fragment html->text first-sentences doc-url suggest-names name-tokens)

(define worker-source #<<WORKER
#lang racket/base
(require setup/xref scribble/xref scribble/manual-struct racket/list racket/string racket/file)
(define args (vector->list (current-command-line-arguments)))
(define cmd (car args))
(define xref (load-collections-xref))

(define (entries)
  (for/list ([e (xref-index xref)] #:when (exported-index-desc? (entry-desc e))) e))
(define (name-of e) (exported-index-desc-name (entry-desc e)))
(define (libs-of e) (filter symbol? (exported-index-desc-from-libs (entry-desc e))))
(define (kind-of e)
  (define d (entry-desc e))
  (define x (and (exported-index-desc*? d) (exported-index-desc*-extras d)))
  (and (hash? x) (hash-ref x 'kind #f)))

(define (arity->datum a)
  (cond [(exact-nonnegative-integer? a) a]
        [(arity-at-least? a) (list '>= (arity-at-least-value a))]
        [(list? a) (map arity->datum a)]
        [else #f]))

;; arity and contract, read inside a namespace that requires the library (contracts are only
;; visible from the instance that made them)
(define (runtime-info lib id)
  (with-handlers ([(λ (e) #t) (λ (e) #f)])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'racket/contract/base)
      (namespace-require lib)
      (define r (eval `(let ([steer-v ,id])
                         (list (procedure? steer-v)
                               (and (procedure? steer-v) (procedure-arity steer-v))
                               (let ([c (value-contract steer-v)]) (and c (format "~s" (contract-name c))))))))
      (list (arity->datum (cadr r)) (caddr r)))))

(define (fragment tag)
  (define-values (path anchor) (xref-tag->path+anchor xref tag))
  (and path anchor (file-exists? path)
       (let* ([s (file->string path)]
              [m (regexp-match-positions (regexp-quote (string-append "name=\"" anchor "\"")) s)]
              [i (and m (caar m))])
         (and i
              (let* ([lo (max 0 (- i 600))]
                     [ps (regexp-match-positions* #rx"<p class=\"RForeground\">" s lo i)]
                     [start (if (pair? ps) (car (last ps)) i)])
                (list (path->string path) anchor (substring s start (min (string-length s) (+ i 14000)))))))))

(define (lib-rank e)                            ; main libraries first; teaching languages last
  (define ls (map symbol->string (libs-of e)))
  (cond [(ormap (λ (l) (regexp-match? #rx"^racket(/|$)" l)) ls) 0]
        [(ormap (λ (l) (regexp-match? #rx"^(lang|htdp|deinprogramm)" l)) ls) 2]
        [else 1]))

;; names in the main racket/* libraries: what a suggestion should come from
(define (main-pool)
  (sort (remove-duplicates (for/list ([e (entries)] #:when (= 0 (lib-rank e))) (symbol->string (name-of e)))) string<?))

(define (lookup id mod)
  (define want (string->symbol id))
  (define hits0 (filter (λ (e) (eq? (name-of e) want)) (entries)))
  (define hits (sort (if mod
                         (let ([m (filter (λ (e) (memq mod (libs-of e))) hits0)]) (if (null? m) hits0 m))
                         hits0)
                     < #:key lib-rank))
  (cond
    [(null? hits) (list 'miss (main-pool))]
    [else
     (list 'ok
           ;; a pool only when nothing in racket/* has this name: the caller warns about the require
           (if (= 0 (lib-rank (car hits))) #f (main-pool))
           (for/list ([e (if (> (length hits) 3) (take hits 3) hits)])
             (define libs (libs-of e))
             (define lib (or mod (and (pair? libs) (car libs))))
             (list (symbol->string (name-of e)) (map symbol->string libs) (kind-of e)
                   (fragment (entry-tag e)) (and lib (runtime-info lib want)))))]))

(define (search words)
  (define ws (map string-downcase words))
  (define hits
    (for/list ([e (entries)]
               #:when (let ([n (string-downcase (symbol->string (name-of e)))])
                        (for/and ([w ws]) (string-contains? n w))))
      (list (symbol->string (name-of e)) (map symbol->string (libs-of e)) (kind-of e))))
  (define exact (and (= (length ws) 1) (car ws)))
  (list 'ok (take (sort hits (λ (a b) (define ea (equal? (string-downcase (car a)) exact))
                               (define eb (equal? (string-downcase (car b)) exact))
                               (cond [(and ea (not eb)) #t] [(and eb (not ea)) #f]
                                     [(< (string-length (car a)) (string-length (car b))) #t]
                                     [(> (string-length (car a)) (string-length (car b))) #f]
                                     [else (string<? (car a) (car b))])))
                  (min 200 (length hits)))))

(define (exports modstr)
  (define mp (if (and (regexp-match? #rx"[.]rkt$" modstr) (file-exists? modstr))
                 (path->complete-path modstr)
                 (string->symbol modstr)))
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require mp #f)
    (define-values (vars stxs) (module->exports mp))
    (define (phase0 l) (let ([p (assv 0 l)]) (if p (map car (cdr p)) '())))
    (define syntax-names (phase0 stxs))
    (list 'ok
          (for/list ([n (sort (remove-duplicates (append (phase0 vars) syntax-names)) symbol<?)])
            ;; contract-out exports appear among the syntax names: only what fails at runtime is a macro
            (define info (runtime-info mp n))
            (define macro? (and (memq n syntax-names) (not info)))
            (list (symbol->string n) (cond [macro? 'macro] [(and info (car info)) 'procedure] [else 'value])
                  (and info (car info)) (and info (cadr info))
                  (and (symbol? mp) (xref-binding->definition-tag xref (list mp n) 0) #t))))))

(write
 (with-handlers ([(λ (e) #t) (λ (e) (list 'error (if (exn? e) (car (regexp-split #rx"\n" (exn-message e))) (format "~s" e))))])
   (when (null? (entries)) (raise (exn:fail "no-docs" (current-continuation-marks))))
   (case cmd
     [("lookup") (lookup (cadr args) (and (pair? (cddr args)) (string->symbol (caddr args))))]
     [("search") (search (cdr args))]
     [("exports") (exports (cadr args))]
     [else (list 'error "unknown worker command")])))
WORKER
  )

(define (shell-quote s) (string-append "'" (string-replace s "'" "'\\''") "'"))

;; Runs the worker in the installed racket. → the datum it wrote. Raises exn:steer on failure.
(define (run-worker args #:timeout [timeout 90])
  (define racket (or (getenv "STEER_RACKET") (let ([p (find-executable-path "racket")]) (and p (path->string p)))))
  (unless racket
    (fail! 'no-racket "doc lookup needs an installed `racket` and its documentation" #:hint "install Racket (full distribution) or set STEER_RACKET" #:code 3))
  (define dir (make-temporary-directory "steer-doc~a"))
  (define worker (build-path dir "worker.rkt"))
  (call-with-output-file worker (λ (o) (void (write-string worker-source o))))
  (define-values (p out in err) (apply subprocess #f #f #f racket (path->string worker) args))
  (close-output-port in)
  (define out-text (box ""))
  (define err-text (box ""))
  (define t1 (thread (λ () (set-box! out-text (port->string out)))))
  (define t2 (thread (λ () (set-box! err-text (port->string err)))))
  (define finished? (sync/timeout timeout p))
  (unless finished? (subprocess-kill p #t))
  (subprocess-wait p)
  (thread-wait t1) (thread-wait t2)
  (close-input-port out) (close-input-port err)
  (delete-directory/files dir)
  (cond
    [(not finished?) (fail! 'timeout (format "doc worker exceeded ~as" timeout) #:hint "the documentation index is large; retry, or pass MODULE to narrow" #:code 3)]
    [(not (eqv? (subprocess-status p) 0))
     (fail! 'doc-worker (format "doc worker failed: ~a" (clip (one-line (unbox err-text)) 300)) #:code 3)]
    [else
     (define d (with-handlers ([exn:fail? (λ (e) #f)]) (parameterize ([read-accept-reader #f]) (read (open-input-string (unbox out-text))))))
     (cond
       [(and (pair? d) (eq? (car d) 'error))
        (if (equal? (cadr d) "no-docs")
            (fail! 'no-docs "no Racket documentation index found on this machine" #:hint "install the full Racket distribution (or `raco setup` after installing the docs)" #:code 3)
            (fail! 'doc-worker (format "doc lookup failed: ~a" (cadr d)) #:code 3))]
       [(not d) (fail! 'doc-worker "doc worker returned nothing readable" #:code 3)]
       [else d])]))

;; ---------------------------------------------------------------------------------------------
;; HTML → text (pure, tested on fixtures)

(define (html->text s)
  (define no-tags (regexp-replace* #rx"<[^>]*>" s ""))
  (define entities
    (for/fold ([t no-tags]) ([e '(("&nbsp;" . " ") ("&rarr;" . "→") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"")
                                  ("&#39;" . "'") ("&hellip;" . "…") ("&ndash;" . "–") ("&mdash;" . "—") ("&amp;" . "&"))])
      (string-replace t (car e) (cdr e))))
  (define numeric (regexp-replace* #rx"&#([0-9]+);" entities (λ (_all n) (string (integer->char (string->number n))))))
  (string-normalize-spaces numeric))

;; First sentences of `s`, at most `limit` characters, cut at a sentence end when possible.
(define (first-sentences s [limit 240])
  (cond
    [(<= (string-length s) limit) s]
    [else
     (define head (substring s 0 limit))
     (define m (regexp-match-positions* #rx"[.] " head))
     (if (pair? m) (string-trim (substring head 0 (cdr (last m)))) (string-append (substring head 0 (- limit 1)) "…"))]))

;; The docs HTML for one definition. `frag` starts at the definition's `<p class="RForeground">`.
;; → (values signature-line argument-lines description); each may be #f / '().
(define (parse-doc-fragment frag)
  (define sig-m (regexp-match-positions #rx"^<p class=\"RForeground\">(.*?)</p>" frag))
  (define sig (and sig-m (html->text (substring frag (car (cadr sig-m)) (cdr (cadr sig-m))))))
  (define after-sig (if sig-m (substring frag (cdar sig-m)) frag))
  (define args
    (let loop ([rest (regexp-replace #rx"^</blockquote></td></tr>" after-sig "")] [acc '()])
      (define m (and (< (length acc) 8) (regexp-match #rx"^<tr><td>(.*?)</td></tr>" rest)))
      (if m
          (loop (substring rest (string-length (car m))) (cons (html->text (cadr m)) acc))
          (reverse acc))))
  (define desc-m (regexp-match #rx"</table></blockquote></div><div class=\"SIntrapara\">(.*?)</div>" frag))
  (values sig args (and desc-m (let ([t (html->text (cadr desc-m))]) (and (not (string=? t "")) t)))))

;; Installed docs live under <racket>/doc/...; the same page is at docs.racket-lang.org.
(define (doc-url path anchor)
  (define m (regexp-match #rx"/doc/(.*)$" path))
  (and m (string-append "https://docs.racket-lang.org/" (cadr m)
                        (if anchor (string-append "#" anchor) ""))))

;; ---------------------------------------------------------------------------------------------
;; Suggestions for names that do not exist. Lexical distance alone cannot map `string-starts-with?` to
;; `string-prefix?`, so three signals are combined: (1) other-language phrases mapped to Racket ones,
;; (2) shared name tokens (`split-string` vs `string-split`), (3) edit distance. Candidates come from
;; racket/* only, so suggestions are usable with a plain `#lang racket`.
;; The table is data, meant to grow from the failure taxonomy (T7). It was seeded from Python, JS,
;; Scheme and Common Lisp habits and from the T3 mutation list, so that list is a dev set, not a test.

(define synonyms
  '(("starts-with" "prefix") ("startswith" "prefix") ("ends-with" "suffix") ("endswith" "suffix")
    ("str" "string") ("concat" "append" "join") ("len" "length") ("size" "length") ("count" "length")
    ("get" "ref") ("put" "set") ("fold" "foldl" "foldr") ("reduce" "foldl") ("select" "filter")
    ("unique" "remove-duplicates") ("distinct" "remove-duplicates") ("upper" "upcase") ("lower" "downcase")
    ("to-upper" "upcase") ("to-lower" "downcase") ("strip" "trim") ("includes" "contains") ("index" "find")
    ("push" "cons") ("float" "exact->inexact") ("to-float" "exact->inexact") ("to-string" "->string")
    ("foreach" "for-each") ("mapcar" "map") ("setq" "set!") ("defun" "define")))

;; whole-name synonyms (the entire identifier)
(define whole-synonyms
  '(("nth" "list-ref") ("sum" "+" "apply") ("concat" "append" "string-append") ("len" "length")
    ("1+" "add1") ("1-" "sub1") ("-1+" "sub1") ("mapcar" "map") ("push" "cons") ("pop" "rest")
    ("print" "displayln") ("puts" "displayln") ("println" "displayln") ("null" "null?" "empty?")
    ("reduce" "foldl") ("fold" "foldl") ("select" "filter") ("unique" "remove-duplicates")))

(define (name-tokens s)
  (filter (λ (t) (not (string=? t "")))
          (regexp-split #rx"[-/>?!*<=]+" (string-downcase s))))

(define (jaccard a b)
  (define sa (remove-duplicates a)) (define sb (remove-duplicates b))
  (define inter (length (filter (λ (x) (member x sb)) sa)))
  (define union (- (+ (length sa) (length sb)) inter))
  (if (zero? union) 0 (/ inter union)))

;; variants of `id` with each synonym phrase replaced, and with ?/! toggled
(define (variants id)
  (define low (string-downcase id))
  (define phrase-variants
    (for*/list ([syn synonyms]
                #:when (regexp-match? (pregexp (string-append "(^|-)" (regexp-quote (car syn)) "($|-|[?!])")) low)
                [alt (cdr syn)])
      (regexp-replace (pregexp (string-append "(^|-)" (regexp-quote (car syn)) "($|-|[?!])")) low
                      (λ (_all pre post) (string-append pre alt (if (member post '("?" "!")) post post))))))
  (define whole (let ([w (assoc low whole-synonyms)]) (if w (cdr w) '())))
  ;; `is-string?`, `is-empty` (Java/JS/Python habit) are predicates: `string?`, `empty?`
  (define is-form (let ([m (regexp-match #rx"^is-(.+?)[?]?$" low)]) (if m (list (string-append (cadr m) "?") (cadr m)) '())))
  (remove-duplicates
   (append whole is-form phrase-variants
           (append* (for/list ([v (cons low phrase-variants)])
                      (list v (string-append v "?") (string-append v "!")
                            (regexp-replace #rx"[?!]$" v "")))))))

(define type-words '("list" "string" "hash" "vector" "dict" "set" "char" "number" "integer" "float" "symbol" "bytes" "array" "seq" "sequence"))

;; `list-sort` → "sort"; #f when there is no leading type word or nothing follows it
(define (verb-of low)
  (define m (regexp-match (pregexp (string-append "^(" (string-join type-words "|") ")-(.+)$")) low))
  (and m (caddr m)))

;; a bare type name (`list`, `list?`, `hash*`) says nothing about what to do with it
(define (bare-type? name)
  (define ts (name-tokens name))
  (and (= (length ts) 1) (member (car ts) type-words) #t))       ; `hash-set` is an operation, `hash` is not

(define (suggest-names id pool #:max [n 5])
  (define low (string-downcase id))
  (define usable (filter (λ (p) (not (bare-type? p))) pool))
  (define full-set (for/hash ([p pool]) (values (string-downcase p) p)))
  (define usable-set (for/hash ([p usable]) (values (string-downcase p) p)))
  (define ((lookup-in set) vs) (for*/list ([v vs] [hit (in-value (hash-ref set v #f))] #:when (and hit (not (string=? v low)))) hit))
  (define lookup-all (lookup-in full-set))            ; whole-name synonyms may be bare types (is-string? → string?)
  (define lookup-usable (lookup-in usable-set))
  (define verb (verb-of low))
  ;; 1. whole-name and phrase synonyms; 2. the same for the verb part alone (list-reduce → reduce → foldl)
  (define strong (lookup-all (variants id)))
  (define verb-strong (if verb (lookup-usable (variants verb)) '()))
  ;; 3. shared verb tokens, then shared tokens overall
  (define id-toks (name-tokens (or verb id)))
  (define (cand-toks p) (name-tokens (or (verb-of (string-downcase p)) p)))
  (define (score p)                                    ; verb part alone, or the whole names
    (max (jaccard id-toks (cand-toks p)) (jaccard (name-tokens id) (name-tokens p))))
  (define token-hits
    (let ([scored (for*/list ([p usable] [j (in-value (score p))] #:when (>= j 1/2))
                    (cons (+ j (if (and verb (equal? (name-tokens p) id-toks)) 1/4 0)) p))])
      (map cdr (sort scored (λ (a b) (or (> (car a) (car b))
                                        (and (= (car a) (car b)) (< (string-length (cdr a)) (string-length (cdr b))))))))))
  ;; 4. edit distance
  (define near (closest id usable #:max 5))
  (define all (remove-duplicates (append strong verb-strong token-hits near)))
  (take all (min n (length all))))

;; ---------------------------------------------------------------------------------------------
;; Commands

(define (arity-text a)
  (cond [(not a) #f]
        [(exact-integer? a) (number->string a)]
        [(and (pair? a) (eq? (car a) '>=)) (format "~a+" (cadr a))]
        [(list? a) (string-join (map arity-text a) "|")]
        [else (format "~a" a)]))

(define (kind-text k) (if (string? k) k "binding"))

(define (require-text libs)
  (if (null? libs) "" (format "(require ~a)" (car libs))))

(define (hit->text h full?)
  (define-values (name libs kind frag info) (apply values h))
  (define-values (sig args desc) (if frag (parse-doc-fragment (caddr frag)) (values #f '() #f)))
  (string-join
   (filter values
           (list (format "~a · ~a · ~a" name (kind-text kind) (require-text libs))
                 (and sig (string-append "  " sig))
                 (and (pair? args) (string-append "  " (string-join args "  ·  ")))
                 (and desc (string-append "  " (if full? desc (first-sentences desc))))
                 (and info (car info) (format "  runtime: arity ~a" (arity-text (car info))))
                 (and info (cadr info) (format "  contract: ~a" (cadr info)))
                 (and frag (let ([u (doc-url (car frag) (cadr frag))]) (and u (string-append "  docs: " u))))))
   "\n"))

(define (hit->data h)
  (define-values (name libs kind frag info) (apply values h))
  (define-values (sig args desc) (if frag (parse-doc-fragment (caddr frag)) (values #f '() #f)))
  (hasheq 'name name 'libs libs 'kind kind 'signature sig 'arguments args 'description desc
          'arity (and info (arity-text (car info))) 'contract (and info (cadr info))
          'docs (and frag (doc-url (car frag) (cadr frag)))))

(define (cmd-doc argv)
  (define-values (pos o) (parse-args "doc" argv '() #:min 2
                                     #:usage "steer doc exists|sig ID [MODULE] | search WORD... | exports MODULE"))
  (define action (car pos))
  (case action
    [("exists" "sig")
     (when (> (length pos) 3) (fail! 'usage (format "`steer doc ~a` takes ID and an optional MODULE" action)))
     (define id (cadr pos))
     (define mod (and (= (length pos) 3) (caddr pos)))
     (define r (run-worker (append (list "lookup" id) (if mod (list mod) '()))))
     (case (car r)
       [(ok)
        (define pool (cadr r))                      ; non-#f: no racket/* library has this name
        (define hits (caddr r))
        (define outside
          (and pool (finding 'warning 'not-in-racket
                             (format "`~a` is not in racket/*; it is only in ~a, which a plain `#lang racket` program does not load"
                                     id (string-join (remove-duplicates (append* (map cadr hits))) ", "))
                             #:fix (let ([near (suggest-names id pool)])
                                     (if (pair? near)
                                         (format "require it explicitly, or use ~a" (string-join near " or "))
                                         "require it explicitly")))))
        (make-reply "doc"
                    (if (equal? action "sig")
                        (string-join (map (λ (h) (hit->text h (current-full?))) hits) "\n\n")
                        (let ([h (car hits)] [others (remove-duplicates (append* (map cadr (cdr hits))))])
                          (string-append (format "yes: ~a · ~a · ~a" (car h) (kind-text (caddr h)) (require-text (cadr h)))
                                         (if (null? others) "" (format "  (also in: ~a)" (string-join (take others (min 4 (length others))) ", "))))))
                    (hasheq 'exists #t 'in_racket (not pool) 'hits (map hit->data hits))
                    #:findings (if outside (list outside) '()))]
       [(miss)
        (define near (suggest-names id (cadr r)))
        (make-reply "doc" "" (hasheq 'exists #f 'suggestions near) #:ok? #f
                    #:findings (list (finding 'error 'unknown-id
                                              (format "`~a` is not documented in any installed library" id)
                                              #:fix (if (pair? near)
                                                        (format "closest names: ~a (`steer doc sig NAME` shows the signature)" (string-join near ", "))
                                                        "check the spelling, or `steer doc search WORD` to browse"))))])]
    [("search")
     (define words (cdr pos))
     (define r (run-worker (cons "search" words)))
     (define hits (cadr r))
     (define cap (or (current-limit) (if (current-full?) +inf.0 15)))
     (define shown (if (> (length hits) cap) (take hits (inexact->exact cap)) hits))
     (make-reply "doc"
                 (string-join
                  (cons (format "~a name~a match ~a" (length hits) (plural (length hits)) (string-join words " "))
                        (append (for/list ([h shown]) (format "  ~a · ~a · ~a" (car h) (kind-text (caddr h)) (require-text (cadr h))))
                                (if (> (length hits) (length shown)) (list (format "  (+~a more: add words, or --limit N)" (- (length hits) (length shown)))) '())))
                  "\n")
                 (hasheq 'count (length hits) 'hits (for/list ([h shown]) (hasheq 'name (car h) 'libs (cadr h) 'kind (caddr h)))))]
    [("exports")
     (when (> (length pos) 2) (fail! 'usage "`steer doc exports` takes one MODULE"))
     (define r (run-worker (list "exports" (cadr pos))))
     (define es (cadr r))
     (define cap (or (current-limit) (if (current-full?) +inf.0 60)))
     (define shown (if (> (length es) cap) (take es (inexact->exact cap)) es))
     (make-reply "doc"
                 (string-join
                  (cons (format "~a provides ~a name~a" (cadr pos) (length es) (plural (length es)))
                        (append (for/list ([e shown])
                                  (define-values (n kind arity contract doc?) (apply values e))
                                  (format "  ~a · ~a~a~a~a" n kind (if arity (format " arity ~a" (arity-text arity)) "")
                                          (if contract (format " ~a" contract) "") (if doc? "" " (undocumented)")))
                                (if (> (length es) (length shown)) (list (format "  (+~a more; --limit N or --full)" (- (length es) (length shown)))) '())))
                  "\n")
                 (hasheq 'module (cadr pos) 'count (length es)
                         'exports (for/list ([e shown]) (hasheq 'name (car e) 'kind (cadr e) 'arity (arity-text (caddr e)) 'contract (cadddr e) 'documented (list-ref e 4)))))]
    [else (fail! 'usage (format "unknown doc action ~a" action) #:hint "exists | sig | search | exports")]))
