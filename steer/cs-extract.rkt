#lang racket/base
;; The C# implementation of lang.rkt's extract/resolve-import graph-ir contract (T63). A body walker
;; over each cs-member's/cs-type's own token range (csharp.rkt's cs-lex/scan-members/scan-types do the
;; hard lexing and frame-tracking; nothing here re-parses C#) emitting calls (`M(`, `this.M(`, `base.M(`,
;; `Type.M(`, `new T(`) with arity, each type's base/interface list (from scan-types), attributes (from
;; scan-types, types only - see the note below), `using` directives as imports, and has-statements?.
;; No dotnet SDK is used or needed, same as the rest of this module's C# support.
;;
;; Known, deliberate scope cuts (recorded per the project's own convention of writing these down
;; rather than silently shipping a narrower thing than the tracked task describes):
;;  - Field/parameter DECLARED TYPES are not tracked for a `declared`-confidence receiver resolution
;;    (e.g. resolving `g.Method()` to `IGuard.Method` because a parameter `IGuard g` is in scope).
;;    That needs a new receiver-hint kind and local-variable/parameter type tracking; only self/base
;;    receivers get that treatment here, like every other language's extractor.
;;  - Member-level attributes are not extracted (only TYPE-level ones): cs-member's own [start,end]
;;    range begins AFTER attributes/modifiers are already skipped by scan-members, so a member's
;;    attribute tokens are not available without re-scanning from the member's ORIGINAL (pre-skip)
;;    position, which scan-members does not preserve.
;;  - resolve-import (`using X`) can only match a project file whose PATH mirrors namespace X as a
;;    folder path (a common but not universal .NET convention) - the lang.rkt resolve-import contract
;;    is path-only (no access to other files' declared namespaces), so a `using` that does not follow
;;    this convention resolves as `uses`/external, and the CALL sites it would have explained instead
;;    fall through to the graph's project-wide name-match - the same over-approximation any language
;;    uses once nothing more specific connects a call to a definition (notes/12, notes/16).
(require racket/list racket/string racket/path
         "csharp.rkt" "graph.rkt")
(provide cs-extract cs-resolve-import)

;; ---------------------------------------------------------------------------------------------
;; small, self-contained token helpers (deliberately not reusing csharp.rkt's private ones, to avoid
;; growing that module's exports just for this)

(define (k tv i) (ctok-kind (vector-ref tv i)))
(define (tx tv i) (ctok-text (vector-ref tv i)))
(define (id? tv i) (and (< i (vector-length tv)) (eq? (k tv i) 'id)))
(define (id-text? tv i s) (and (id? tv i) (equal? (tx tv i) s)))
(define (p? tv i s) (and (< i (vector-length tv)) (eq? (k tv i) 'punct) (equal? (tx tv i) s)))

(define (skip-group tv i open close)
  (let loop ([i (add1 i)] [d 1])
    (cond [(>= i (vector-length tv)) i]
          [(p? tv i open) (loop (add1 i) (add1 d))]
          [(p? tv i close) (if (= d 1) (add1 i) (loop (add1 i) (sub1 d)))]
          [else (loop (add1 i) d)])))
(define (skip-generic tv i) (skip-group tv i "<" ">"))

;; tv[open] = "(" → (values arity close-index)
(define (count-args tv open)
  (define close (skip-group tv open "(" ")"))
  (define last (sub1 close))
  (cond
    [(= (add1 open) last) (values 0 close)]
    [else
     (let loop ([i (add1 open)] [d 0] [n 1] [seen-tok? #f])
       (cond
         [(= i last) (values n close)]
         [(member (tx tv i) '("(" "[" "<")) (loop (add1 i) (add1 d) n #t)]
         [(member (tx tv i) '(")" "]" ">")) (loop (add1 i) (max 0 (sub1 d)) n #t)]
         [(and (zero? d) (p? tv i ",")) (loop (add1 i) d (add1 n) #f)]
         [else (loop (add1 i) d n #t)]))]))

;; call/decorator-style keywords that are never themselves a project symbol to resolve
(define control-keywords
  '("if" "while" "for" "foreach" "switch" "catch" "using" "lock" "fixed" "checked" "unchecked"
    "return" "throw" "typeof" "sizeof" "nameof" "default" "when" "is" "as" "await" "yield" "new"))

;; A member's [start,end] range begins at its RETURN TYPE (cs-member-start is set before the name is
;; even seen), so `Speak()`'s own declaration head (`string Speak(...)`) reads exactly like a call to
;; `Speak` unless the ref scan starts AFTER the signature, at the body's first token (right after the
;; first depth-0 `{` or `=>`). A declaration with no body (`;` only - abstract/interface/partial) has
;; nothing to scan at all: (add1 end), not `start`, so its signature is never mistaken for a call either.
(define (member-body-start tv start end)
  (let loop ([i start] [pd 0])
    (cond [(> i end) (add1 end)]
          [(member (tx tv i) '("(" "[")) (loop (add1 i) (add1 pd))]
          [(member (tx tv i) '(")" "]")) (loop (add1 i) (max 0 (sub1 pd)))]
          [(and (zero? pd) (p? tv i "{")) (add1 i)]
          [(and (zero? pd) (p? tv i "=>")) (add1 i)]
          [else (loop (add1 i) pd)])))

;; ---------------------------------------------------------------------------------------------
;; refs: a linear scan over [start,end] (inclusive), the exact shapes M(, this.M(, base.M(, Type.M(,
;; new T( ask for.

;; Every match records itself and steps by (add1 i), NEVER jumps to the call's closing paren: a call
;; argument can itself contain a call (`Console.WriteLine(d.Speak())`), and only a token-by-token walk
;; finds it. `count-args` is used only to compute arity, its `close` result is otherwise unused here.
(define (scan-refs tv start end scope)
  (define out '())
  (define (add! kind name recv arity line) (set! out (cons (ref kind name recv scope arity line) out)))
  (let loop ([i start])
    (when (<= i end)
      (cond
        [(and (id-text? tv i "this") (p? tv (add1 i) ".") (id? tv (+ i 2)) (p? tv (+ i 3) "("))
         (define-values (arity _close) (count-args tv (+ i 3)))
         (add! 'call (tx tv (+ i 2)) 'self arity (ctok-line (vector-ref tv i)))
         (loop (add1 i))]
        [(and (id-text? tv i "base") (p? tv (add1 i) ".") (id? tv (+ i 2)) (p? tv (+ i 3) "("))
         (define-values (arity _close) (count-args tv (+ i 3)))
         (add! 'call (tx tv (+ i 2)) 'base arity (ctok-line (vector-ref tv i)))
         (loop (add1 i))]
        [(and (id-text? tv i "new") (id? tv (add1 i)))
         (define after (if (p? tv (+ i 2) "<") (skip-generic tv (+ i 2)) (+ i 2)))
         (when (p? tv after "(")
           (define-values (arity _close) (count-args tv after))
           (add! 'call (tx tv (add1 i)) #f arity (ctok-line (vector-ref tv i))))
         (loop (add1 i))]
        ;; a plain call, but not the tail of a dotted chain already handled by one of the patterns
        ;; above (this.M(/base.M(/X.M(): those record `M` once when they see the RECEIVER token;
        ;; stepping token-by-token (not jumping past the call) means this pattern would otherwise see
        ;; `M(` again right after and double-count it.
        [(and (id? tv i) (p? tv (add1 i) "(") (not (member (tx tv i) control-keywords))
              (or (= i start) (not (or (p? tv (sub1 i) ".") (id-text? tv (sub1 i) "new")))))
         (define-values (arity _close) (count-args tv (add1 i)))
         (add! 'call (tx tv i) #f arity (ctok-line (vector-ref tv i)))
         (loop (add1 i))]
        [(and (id? tv i) (p? tv (add1 i) ".") (id? tv (+ i 2)) (p? tv (+ i 3) "(")
              (not (member (tx tv i) '("this" "base"))) (not (member (tx tv i) control-keywords)))
         (define-values (arity _close) (count-args tv (+ i 3)))
         (add! 'call (tx tv (+ i 2)) #f arity (ctok-line (vector-ref tv i)))
         (loop (add1 i))]
        [else (loop (add1 i))])))
  (reverse out))

;; ---------------------------------------------------------------------------------------------
;; extract

(define entry-comment-rx #px"//\\s*steer:\\s*entry\\s*$")
(define (entry-lines text)
  (define lines (string-split text "\n" #:trim? #f))
  (for/list ([l lines] [i (in-naturals 1)] #:when (regexp-match? entry-comment-rx l)) (add1 i)))

;; a bare (unqualified) type/member kind, matching Racket/Python's vocabulary where it lines up:
;; a member named the same as its enclosing type is its constructor.
(define (member-kind->def-kind m enclosing-type-name)
  (define bare (last (string-split (cs-member-qualname m) ".")))
  (cond
    [(and (equal? (cs-member-kind m) "method") (equal? bare enclosing-type-name)) 'constructor]
    [(equal? (cs-member-kind m) "method") 'method]
    [(equal? (cs-member-kind m) "property") 'property]
    [(equal? (cs-member-kind m) "indexer") 'property]
    [(equal? (cs-member-kind m) "field") 'field]
    [(equal? (cs-member-kind m) "operator") 'method]
    [(equal? (cs-member-kind m) "destructor") 'method]
    [else 'method]))

(define (type-kind->def-kind s)
  (cond [(equal? s "interface") 'interface] [(equal? s "struct") 'struct] [(equal? s "enum") 'enum] [else 'class]))

(define (cs-extract text path)
  (define-values (tokens err) (cs-lex text))
  (cond
    [err (file-facts path 'csharp '() '() '() #f (short-text-hash text))]
    [else
     (define tv (list->vector tokens))
     (define n (vector-length tv))
     (define entries (entry-lines text))
     (define (entry-here? line) (and (memq line entries) #t))
     (define types (scan-types tokens))
     (define members (scan-members tokens))
     ;; the type that directly encloses a qualname: the type whose own qualname is the longest
     ;; strict dotted prefix, or the qualname itself minus its last segment
     (define (enclosing-type-of qualname)
       (define segs (string-split qualname "."))
       (and (> (length segs) 1) (string-join (drop-right segs 1) ".")))
     (define type-defs
       (for/list ([ty types])
         (define scope (enclosing-type-of (cs-type-qualname ty)))
         (define line (ctok-line (vector-ref tv (cs-type-start ty))))
         (define endl (ctok-eline (vector-ref tv (cs-type-end ty))))
         (def (type-kind->def-kind (cs-type-kind ty)) (last (string-split (cs-type-qualname ty) ".")) (cs-type-qualname ty)
              scope line endl (format "~a ~a" (cs-type-kind ty) (cs-type-qualname ty)) (tokens-hash tv (cs-type-start ty) (cs-type-end ty))
              (cs-type-bases ty) (cs-type-attrs ty) (entry-here? line) #t)))
     (define member-defs
       (for/list ([m members])
         (define scope (enclosing-type-of (cs-member-qualname m)))
         (define enclosing-name (last (string-split (or scope (cs-member-qualname m)) ".")))
         (define line (ctok-line (vector-ref tv (cs-member-start m))))
         (define endl (ctok-eline (vector-ref tv (cs-member-end m))))
         (def (member-kind->def-kind m enclosing-name) (last (string-split (cs-member-qualname m) ".")) (cs-member-qualname m)
              scope line endl (format "~a ~a" (cs-member-kind m) (cs-member-qualname m)) (tokens-hash tv (cs-member-start m) (cs-member-end m))
              '() '() (entry-here? line) #t)))
     (define member-refs
       (append*
        (for/list ([m members])
          (define bstart (member-body-start tv (cs-member-start m) (cs-member-end m)))
          (scan-refs tv bstart (cs-member-end m) (cs-member-qualname m)))))
     ;; top-level refs: any token span NOT covered by a member's own [start,end] is scanned at module
     ;; scope (scope=#f) - a type's base-list call (`record Dog(int x) : Animal(x)`), a top-level
     ;; C# 9+ statement, anything outside a member body. Gaps, not "the whole file minus ranges" one
     ;; call, so refs already attributed to a member's scope are never double-counted at scope=#f too.
     (define member-ranges (sort (for/list ([m members]) (cons (cs-member-start m) (cs-member-end m))) < #:key car))
     (define gaps
       (let loop ([prev 0] [ranges member-ranges] [acc '()])
         (cond [(null? ranges) (if (<= prev (sub1 n)) (cons (cons prev (sub1 n)) acc) acc)]
               [else (define r (car ranges))
                     (define acc2 (if (< prev (car r)) (cons (cons prev (sub1 (car r))) acc) acc))
                     (loop (add1 (cdr r)) (cdr ranges) acc2)])))
     (define top-refs (append* (for/list ([g (reverse gaps)] #:when (<= (car g) (cdr g))) (scan-refs tv (car g) (cdr g) #f))))
     (define usings (scan-usings tokens))
     (define imports (for/list ([u usings]) (import (car u) (cadr u) (caddr u))))
     ;; has-statements?: no C# type at all, but real tokens - the rare C# 9+ top-level-statements form
     (define has-stmt? (and (null? types) (pair? tokens)))
     (file-facts path 'csharp (append type-defs member-defs) (append member-refs top-refs) imports has-stmt? (short-text-hash text))]))

(define (tokens-hash tv start end)
  (short-sha1 (format "~s" (for/list ([i (in-range start (add1 end))]) (cons (ctok-kind (vector-ref tv i)) (ctok-text (vector-ref tv i)))))))
(define (short-text-hash text) (short-sha1 (string-normalize-spaces text)))
(define (hexb bs) (apply string-append (for/list ([b bs]) (string-append (if (< b 16) "0" "") (number->string b 16)))))
(define (short-sha1 s) (substring (hexb (sha1-bytes (open-input-bytes (string->bytes/utf-8 s)))) 0 12))

;; ---------------------------------------------------------------------------------------------
;; resolve-import: `using X` maps to a project file whose path mirrors X as a folder path (a common
;; but not universal .NET convention - see the module comment for what happens when it does not).
(define (cs-resolve-import lang spec importing-path root all-paths)
  (cond
    [(not (string? spec)) '()]
    [else
     (define rel (string-replace spec "." "/"))
     (define want (string-append rel ".cs"))
     (filter (λ (p) (let ([pf (string-replace (if (path? p) (path->string p) p) "\\" "/")])
                      (or (equal? pf want) (string-suffix? pf (string-append "/" want)))))
             all-paths)]))
