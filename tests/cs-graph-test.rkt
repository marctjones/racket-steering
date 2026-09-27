#lang racket/base
;; T63: the C# extractor implementing lang.rkt's generic extract/resolve-import contract, over the
;; two-file fixtures/csproj/{Shapes,Helper}.cs project. No dotnet SDK needed. Same five-list
;; conformance pattern XL1's tests/cs-anchors-test.rkt used (must-find-edges, known-invisible,
;; must-find-entries, must-not-be-dead, plus a hash-identity check against the existing cs-find-anchor).
(require rackunit racket/list racket/string racket/file racket/runtime-path racket/path
         "../steer/graph.rkt" "../steer/cs-extract.rkt" "../steer/csharp.rkt" "../steer/anchors.rkt" "../steer/lang.rkt")

(define-runtime-path shapes-cs "fixtures/csproj/Shapes.cs")
(define-runtime-path helper-cs "fixtures/csproj/Helper.cs")
(define dir (path-only shapes-cs))

(define shapes-text (file->string shapes-cs))
(define helper-text (file->string helper-cs))
(define fs (cs-extract shapes-text "Shapes.cs"))
(define fh (cs-extract helper-text "Helper.cs"))
(define g (link-facts (list fs fh) #:root dir #:resolve-import cs-resolve-import))

(define (has-edge? from to kind conf [arity #f] [ext? #f]) (and (member (gedge from to kind conf arity ext?) (graph-edges g)) #t))

;; ---------------------------------------------------------------------------------------------
;; 1. must-find-edges: same-file exact, self/base receiver resolution, `new T()` as a constructor-
;; site call, inherits across a 3-level chain, chained overrides (LoudDog -> Dog -> Animal, each hop
;; to its DIRECT parent - a real bug found and fixed in graph.rkt's overrides logic while building
;; this fixture: it used to match the bare name against ANY def in the file, picking whichever
;; same-named method came first in file order, e.g. Animal.Speak, instead of the real direct parent),
;; and the project-wide name-match fallback a `using` with no dotnet SDK forces (no namespace index
;; to resolve it against - cs-extract.rkt's module comment explains why).

(define must-find-edges
  (list (list "Shapes.cs#Dog.Speak" "Shapes.cs#Dog.Bark" 'calls 'exact 0)
        (list "Shapes.cs#LoudDog.Speak" "Shapes.cs#Animal.Speak" 'calls 'declared 0)
        (list "Shapes.cs#Program.Run" "Shapes.cs#Dog" 'calls 'exact 0)           ; new Dog()
        (list "Shapes.cs#Dog" "Shapes.cs#Animal" 'inherits 'exact #f)
        (list "Shapes.cs#LoudDog" "Shapes.cs#Dog" 'inherits 'exact #f)
        (list "Shapes.cs#Dog.Speak" "Shapes.cs#Animal.Speak" 'overrides 'declared #f)
        (list "Shapes.cs#LoudDog.Speak" "Shapes.cs#Dog.Speak" 'overrides 'declared #f)
        (list "Shapes.cs#Dog.Speak" "Helper.cs#Helper.Double" 'calls 'name-match 1)
        (list "Shapes.cs#Program.UnusedHelper" "Helper.cs#Helper.Triple" 'calls 'name-match 1)))
(for ([e must-find-edges]) (check-true (apply has-edge? e) (format "missing edge: ~a" e)))

;; ---------------------------------------------------------------------------------------------
;; 2. known-invisible: a member's own declaration head (return type + name + parameter list) must
;; NEVER be read as a self-call - `public string Speak()` is not a call to `Speak`. This was a real
;; bug found while building this extractor (cs-member's [start,end] range begins at the return type,
;; before the name, so a naive scan-from-start saw `Speak(` and recorded a call to itself).

(check-false (findf (λ (r) (and (equal? (ref-name r) "Speak") (equal? (ref-scope r) "Animal.Speak"))) (file-facts-refs fs))
             "Animal.Speak's own signature is not a call to Speak")
(check-false (findf (λ (r) (and (equal? (ref-name r) "Run") (equal? (ref-scope r) "Program.Run"))) (file-facts-refs fs))
             "Program.Run's own signature is not a call to Run")

;; ---------------------------------------------------------------------------------------------
;; 3. must-find-entries: the `// steer: entry` marker above `Run`

(define run-def (findf (λ (d) (equal? (def-qualname d) "Program.Run")) (file-facts-defs fs)))
(check-true (def-entry? run-def) "Run should be marked entry?")
(check-false (def-entry? (findf (λ (d) (equal? (def-qualname d) "Program.UnusedHelper")) (file-facts-defs fs))))

;; ---------------------------------------------------------------------------------------------
;; 4. must-not-be-dead: a forward walk from the one entry (`Run`), over calls+defines+imports+
;; overrides, reaches every symbol a real call chain touches (including, via the fixed overrides
;; edge, LoudDog.Speak and Dog.Speak once Animal.Speak is - here it is not, so neither is reachable
;; from THIS entry - but UnusedHelper and its callee are the clean, real "would be dead" case).

(define (forward-reachable-from start)
  (let loop ([frontier (list start)] [seen (hash start #t)])
    (define next
      (for*/list ([id frontier] [e (graph-edges g)] #:when (and (equal? (gedge-from e) id) (gedge-to e)) #:unless (hash-ref seen (gedge-to e) #f))
        (gedge-to e)))
    (if (null? next) seen (loop next (for/fold ([s seen]) ([n next]) (hash-set s n #t))))))

(define reachable (forward-reachable-from "Shapes.cs#Program.Run"))
(for ([id '("Shapes.cs#Program.Run" "Shapes.cs#Dog" "Shapes.cs#Dog.Speak" "Shapes.cs#Dog.Bark" "Helper.cs#Helper.Double")])
  (check-true (hash-ref reachable id #f) (format "~a must be reachable from the entry" id)))
(for ([id '("Shapes.cs#Program.UnusedHelper" "Helper.cs#Helper.Triple" "Shapes.cs#LoudDog")])
  (check-false (hash-ref reachable id #f) (format "~a is NOT reached from the one entry (a real dead-code candidate for T65)" id)))

;; ---------------------------------------------------------------------------------------------
;; 5. hash-identity: the graph's def-hash for a symbol equals cs-find-anchor's hash for that symbol -
;; the extractor must use the SAME token-range hashing the anchor resolver already uses

(define bark-def (findf (λ (d) (equal? (def-qualname d) "Dog.Bark")) (file-facts-defs fs)))
(define bark-anchor (resolve-anchor dir "Shapes.cs#Dog.Bark"))
(check-true (hash-ref bark-anchor 'found?))
(check-equal? (def-hash bark-def) (hash-ref bark-anchor 'hash) "graph def-hash and resolve-anchor's hash must agree exactly")
(check-equal? (def-hash run-def) (hash-ref (resolve-anchor dir "Shapes.cs#Program.Run") 'hash))

;; ---------------------------------------------------------------------------------------------
;; kind mapping and attributes

(check-equal? (def-kind (findf (λ (d) (equal? (def-qualname d) "Animal")) (file-facts-defs fs))) 'class)
(check-equal? (def-kind bark-def) 'method)
(check-equal? (def-decorators (findf (λ (d) (equal? (def-qualname d) "Animal")) (file-facts-defs fs))) '("Serializable"))
(check-equal? (def-bases (findf (λ (d) (equal? (def-qualname d) "LoudDog")) (file-facts-defs fs))) '("Dog"))

;; file-facts flags

(check-false (file-facts-has-statements? fs) "every top-level thing in this file is a type declaration")
(check-equal? (file-facts-lang fs) 'csharp)
(check-true (string? (file-facts-content-hash fs)))
(check-equal? (length (file-facts-imports fs)) 1 "one `using System;`")

;; ---------------------------------------------------------------------------------------------
;; the six-function contract: the C# gate now exposes `extract`/`resolve-import`/`dynamic-calls?`
;; (T58 reserved extract/resolve-import as #f; T63 fills them in)

(define cs-gate (gate-for-path "x.cs"))
(check-true (procedure? (gate-extract cs-gate)) "T58's reserved slot is now implemented")
(check-true (procedure? (gate-resolve-import cs-gate)))
(check-true (gate-dynamic-calls? cs-gate) "no dotnet SDK / type inference: calls are resolved via name-match, never claimed exact")
(define fs2 ((gate-extract cs-gate) shapes-text "Shapes.cs"))
(check-equal? fs2 fs "the gate's extract slot IS cs-extract, not a second implementation")

;; ---------------------------------------------------------------------------------------------
;; resolve-import: a `using` that DOES follow the folder=namespace convention still resolves

(check-equal? (cs-resolve-import 'csharp "My.Pkg" "a.cs" "." (list "My/Pkg.cs" "a.cs")) (list "My/Pkg.cs"))
(check-equal? (cs-resolve-import 'csharp "System" "a.cs" "." (list "a.cs")) '() "System has no project file: external, not forced to match")
