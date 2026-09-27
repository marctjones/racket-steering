#lang racket/base
;; T50: exact C# anchors (generics, properties/indexers, operators, destructors, partial classes,
;; attributes, Allman/K&R bodies, overloads) via a member scanner over cs-lex's tokens, replacing the
;; indentation heuristic that note 12 found wrong on exactly these shapes. No dotnet needed.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/anchors.rkt" "../steer/csharp.rkt" racket/path)

(define-runtime-path shapes-cs "fixtures/cs/Shapes.cs")
(define-runtime-path scoped-cs "fixtures/cs/FileScoped.cs")

(define shapes (file->string shapes-cs))
(define scoped (file->string scoped-cs))
(define dir (path-only shapes-cs))
(define (resolve name #:file [f "Shapes.cs"]) (resolve-anchor dir (format "~a#~a" f name)))

;; ---------------------------------------------------------------------------------------------
;; note 12's specific failures, fixed

;; a static property with a trailing initializer (`{ get; } = new Guard();`): span through the `;`,
;; not cut short at the accessor block
(let ([r (resolve "Guard.Against")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'method) 'csharp)
  (check-equal? (hash-ref r 'kind) "property")
  (check-equal? (list (hash-ref r 'line) (hash-ref r 'end)) '(9 9)))

;; overloaded constructors: bare name is ambiguous, `/arity` disambiguates
(check-false (hash-ref (resolve "Guard.Guard") 'found? #f))
(check-regexp-match #rx"2 overloads of Guard.Guard: specify Name/arity or Name\\(types\\)" (hash-ref (resolve "Guard.Guard") 'problem))
(check-regexp-match #rx"Guard.Guard/0, Guard.Guard\\(int\\)" (hash-ref (resolve "Guard.Guard") 'problem))
(let ([r (resolve "Guard.Guard/0")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "method")
  (check-equal? (hash-ref r 'shadowed) 1 "one other overload shares this qualified name"))
(let ([r (resolve "Guard.Guard/1")])
  (check-true (hash-ref r 'found?))
  (check-equal? (list (hash-ref r 'line) (hash-ref r 'end)) '(16 19)))

;; generic method overloaded by arity: Null<T>(input, name) vs Null<T>(input, name, message)
(check-false (hash-ref (resolve "Guard.Null") 'found? #f))
(check-true (hash-ref (resolve "Guard.Null/2") 'found?))
(check-true (hash-ref (resolve "Guard.Null/3") 'found?))
(check-not-equal? (hash-ref (resolve "Guard.Null/2") 'hash) (hash-ref (resolve "Guard.Null/3") 'hash))

;; an expression-bodied property (`=>`), not confused with a field
(let ([r (resolve "Guard.Limit")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "property"))

;; overloads sharing an arity, disambiguated by parameter type
(check-false (hash-ref (resolve "Guard.Describe") 'found? #f))
(check-regexp-match #rx"Guard.Describe\\(int\\), Guard.Describe\\(string\\)" (hash-ref (resolve "Guard.Describe") 'problem))
(let ([r (resolve "Guard.Describe(int)")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'line) 43))
(let ([r (resolve "Guard.Describe(string)")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'line) 45)
  (check-not-equal? (hash-ref r 'hash) (hash-ref (resolve "Guard.Describe(int)") 'hash)))

;; a nested class's method (dotted qualification reaches inside it)
(let ([r (resolve "Guard.Nested.Run")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "method"))

;; a private member inside a #region (the region directive must not hide it, nor confuse the scanner
;; about the class's closing brace)
(check-true (hash-ref (resolve "Guard.Helper") 'found? #f))

;; partial classes: each partial declares a different member, both reachable under the same qualname
(check-true (hash-ref (resolve "GuardExtensions.First") 'found? #f))
(check-true (hash-ref (resolve "GuardExtensions.Second") 'found? #f))
(check-not-equal? (hash-ref (resolve "GuardExtensions.First") 'hash) (hash-ref (resolve "GuardExtensions.Second") 'hash))

;; an interface's members (no body: `;`-terminated)
(check-true (hash-ref (resolve "IGuardClause.Check") 'found? #f))
(check-equal? (hash-ref (resolve "IGuardClause.Name") 'kind) "property")

;; a record's own method
(check-true (hash-ref (resolve "Point.Sum") 'found? #f))

;; a generic struct's fields
(check-true (hash-ref (resolve "Pair.First") 'found? #f))
(check-true (hash-ref (resolve "Pair.Second") 'found? #f))

;; ---------------------------------------------------------------------------------------------
;; FileScoped.cs: file-scoped namespace, indexer, event, operator, finalizer, nested class

(let ([r (resolve "Outer.Count" #:file "FileScoped.cs")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "property"))
(let ([r (resolve "Outer.this[]" #:file "FileScoped.cs")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "indexer"))
(check-equal? (hash-ref (resolve "Outer.Changed" #:file "FileScoped.cs") 'kind) "field")
(let ([r (resolve "Outer.operator+" #:file "FileScoped.cs")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "operator"))
(let ([r (resolve "Outer.~Outer" #:file "FileScoped.cs")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "destructor"))
(let ([r (resolve "Outer.Inner.Value" #:file "FileScoped.cs")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'kind) "method"))

;; ---------------------------------------------------------------------------------------------
;; unresolvable: a typo or a nonexistent member is not-found, never silently "pending"

(check-false (hash-ref (resolve "NotThere") 'found? #f))
(check-false (hash-ref (resolve "Guard.NotThere") 'found? #f))
(check-regexp-match #rx"no definition of Guard.NotThere" (hash-ref (resolve "Guard.NotThere") 'problem))

;; ---------------------------------------------------------------------------------------------
;; the hash: formatting-insensitive (comments, reflow), but a real change is not

(define base (baseline-anchor dir "Shapes.cs#Guard.Limit"))
(check-equal? (anchor-state base (resolve "Guard.Limit")) 'ok)
(define dir2 (make-temporary-directory "steer-csanc~a"))
(define (put! d name text) (call-with-output-file (build-path d name) #:exists 'truncate (λ (o) (void (write-string text o)))))
(put! dir2 "s.cs" (string-replace shapes "public string Describe(int x) => $\"limit={_limit}, x={x}\";"
                                  "public string Describe(int x) =>\n            // a comment\n            $\"limit={_limit}, x={x}\";"))
(define base2 (baseline-anchor dir "Shapes.cs#Guard.Describe(int)"))
(check-equal? (anchor-state base2 (resolve-anchor dir2 "s.cs#Guard.Describe(int)")) 'ok "reflow and a comment do not make it stale")
(put! dir2 "s.cs" (string-replace shapes "public string Describe(int x) => $\"limit={_limit}, x={x}\";"
                                  "public string Describe(int x) => $\"limit={_limit}, x={x}!\";"))
(check-equal? (anchor-state base2 (resolve-anchor dir2 "s.cs#Guard.Describe(int)")) 'changed "a real body change does count")
(delete-directory/files dir2)

;; ---------------------------------------------------------------------------------------------
;; not a C# file at all, and one with a syntax error, are reported (not confused with "pending")

(define dir3 (make-temporary-directory "steer-csanc~a"))
(put! dir3 "bad.cs" "class A { void F() { \"unterminated\n } }\n")
(let ([r (resolve-anchor dir3 "bad.cs#A.F")])
  (check-false (hash-ref r 'found? #f))
  (check-regexp-match #rx"unreadable" (hash-ref r 'problem)))
(delete-directory/files dir3)

;; ---------------------------------------------------------------------------------------------
;; measured: every member the fixture was built to exercise resolves under its qualified name

(define fixture-members
  '("Guard.Against" "Guard.Guard/0" "Guard.Guard/1" "Guard.Null/2" "Guard.Null/3" "Guard.Limit"
    "Guard.Describe(int)" "Guard.Describe(string)" "Guard.Nested.Run" "Guard.Helper"
    "GuardExtensions.First" "GuardExtensions.Second" "IGuardClause.Check" "IGuardClause.Name"
    "Point.Sum" "Pair.First" "Pair.Second"))
(define hits (for/list ([n fixture-members]) (cons n (hash-ref (resolve n) 'found? #f))))
(printf "cs-anchors-test: ~a of ~a fixture members resolve by their qualified name\n"
        (length (filter cdr hits)) (length hits))
(check-equal? (length (filter cdr hits)) (length hits) "every one of them resolves")

;; ---------------------------------------------------------------------------------------------
;; routing

(check-equal? (hash-ref (resolve "Guard.Limit") 'method) 'csharp)
