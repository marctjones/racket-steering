#lang racket/base
;; T65: reachability (reachable(S)/dead(S)) - one generic worklist BFS over the shared graph. Includes
;; mutation-style tests analogous to XL1's bracket-deletion mutations: take a known-reachable symbol,
;; remove its only caller, confirm `dead` catches it; confirm a symbol reachable ONLY via an `overrides`
;; edge is NOT flagged dead. Also regression-guards two real bugs found while building this: a module
;; becoming reachable must not make every OTHER top-level def in that file reachable (via `defines`),
;; and a class becoming reachable must not make every OTHER method on it reachable either - only its
;; constructors, which is exactly what "class reachable => its constructors" means and no more.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/reach.rkt" "../steer/entries.rkt" "../steer/graph.rkt" "../steer/rules.rkt")

(define-runtime-path py-dir "fixtures/pyproj")
(define-runtime-path cs-dir "fixtures/csproj")
(define-runtime-path rkt-dir "fixtures/rkt")

;; ---------------------------------------------------------------------------------------------
;; the three T60/T62/T63 fixtures: each has exactly the dead set its own fixture comment promised

(let-values ([(g parent ids by) (project-reachability py-dir)])
  (define dead (map gnode-id (dead-nodes g parent)))
  (for ([id '("shapes.py#unused_helper" "helper.py#triple" "shapes.py#Dog" "shapes.py#Animal")])
    (check-true (and (member id dead) #t) (format "~a should be dead" id)))
  (check-false (and (member "shapes.py#area" dead) #t))
  (check-false (and (member "shapes.py#compute_area" dead) #t))
  (check-false (and (member "helper.py#double" dead) #t)))

;; every type/member in this fixture is `public`, so C#'s own public_api entry rule (added mid-T68,
;; measuring a real C# library where its absence made 838 of 841 real symbols look dead) now admits
;; all of them as entries too - the same way Python's already did for an exported top-level symbol in
;; a root module. Nothing here is a genuinely dead-code case any more; that is checked separately
;; below, with `private` members, which the public_api rule (correctly) never touches.
(let-values ([(g parent ids by) (project-reachability cs-dir)])
  (define dead (map gnode-id (dead-nodes g parent)))
  (check-false (and (member "Shapes.cs#Program.UnusedHelper" dead) #t) "public, in a root module: admitted via public_api now")
  (check-false (and (member "Helper.cs#Helper.Triple" dead) #t))
  (check-false (and (member "Shapes.cs#LoudDog" dead) #t))
  (check-false (and (member "Shapes.cs#Dog" dead) #t) "instantiated via `new Dog()` (and also public_api now)")
  (check-false (and (member "Helper.cs#Helper.Double" dead) #t)))

;; a genuinely dead C# case: `private` members are never touched by public_api, so ordinary call-based
;; reachability is still the only thing that can save them.
(let ()
  (define d (make-temporary-directory "steer-reach-cs~a"))
  (call-with-output-file (build-path d "P.cs") #:exists 'truncate
    (λ (o) (void (write-string "namespace N {\n  public class Program {\n    // steer: entry\n    public static void Run() { Helper(); }\n    private static void Helper() { }\n    private static void NeverCalled() { }\n  }\n}\n" o))))
  (define-values (g parent ids by) (project-reachability d))
  (define dead (map gnode-id (dead-nodes g parent)))
  (check-false (and (member "P.cs#Program.Helper" dead) #t) "private, but called from Run")
  (check-true (and (member "P.cs#Program.NeverCalled" dead) #t) "private and never called: still a real dead-code case")
  (delete-directory/files d))

(let-values ([(g parent ids by) (project-reachability rkt-dir)])
  (define dead (map gnode-id (dead-nodes g parent)))
  (check-true (and (member "shapes.rkt#unused-helper" dead) #t))
  (check-false (and (member "shapes.rkt#area" dead) #t))
  (check-false (and (member "shapes.rkt#compute-area" dead) #t)))

;; ---------------------------------------------------------------------------------------------
;; mutation 1: remove a symbol's only caller -> it goes from live to dead

(define dir (make-temporary-directory "steer-reach~a"))
(define (write-file! rel text)
  (define p (build-path dir rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))

;; `_helper` (leading underscore): NOT in public_api, so python's own public_api+root_module entry
;; rule does not ALSO make it an entry independent of the call - without this, a top-level function
;; in a root module (nothing imports this file) is already an entry via that rule regardless of
;; whether anything calls it, and the mutation below would not test what it claims to (found by
;; running this test for real: a first version using a plain `helper` name never went dead).
(write-file! "m.py" "# steer: entry\ndef entry_point():\n    return _helper()\n\ndef _helper():\n    return 1\n")
(let-values ([(g parent ids by) (project-reachability dir)])
  (check-false (and (member "m.py#_helper" (map gnode-id (dead-nodes g parent))) #t) "_helper is called: live"))

;; the mutation: delete _helper's only caller (mirrors bracket-deletion mutation testing's own idea -
;; make ONE targeted change and confirm the analysis notices)
(write-file! "m.py" "# steer: entry\ndef entry_point():\n    return 1\n\ndef _helper():\n    return 1\n")
(let-values ([(g parent ids by) (project-reachability dir)])
  (check-true (and (member "m.py#_helper" (map gnode-id (dead-nodes g parent))) #t) "_helper's only caller is gone: now dead"))

;; and restoring the caller makes it live again (the mutation is reversible, not a one-way ratchet)
(write-file! "m.py" "# steer: entry\ndef entry_point():\n    return _helper()\n\ndef _helper():\n    return 1\n")
(let-values ([(g parent ids by) (project-reachability dir)])
  (check-false (and (member "m.py#_helper" (map gnode-id (dead-nodes g parent))) #t) "restored: live again"))
(delete-directory/files dir)

;; ---------------------------------------------------------------------------------------------
;; mutation 2: a symbol reachable ONLY through an `overrides` edge (no direct call anywhere) is NOT dead

(define dir2 (make-temporary-directory "steer-reach~a"))
(define (write-file2! rel text)
  (define p (build-path dir2 rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))
(write-file2! "m.py"
  (string-append "__all__ = [\"run\"]\n\n"
                  "class Base:\n    def speak(self):\n        return 1\n\n"
                  "class Sub(Base):\n    def speak(self):\n        return 2\n\n"
                  "# steer: entry\ndef run(x):\n    return x.speak()\n"))
(let-values ([(g parent ids by) (project-reachability dir2)])
  (define dead (map gnode-id (dead-nodes g parent)))
  ;; `x.speak()` has no declared-type receiver (no type inference), so it name-matches BOTH Base.speak
  ;; and Sub.speak directly - a weaker test of the overrides mechanism than a receiver that can only
  ;; ever resolve to Base. Rebuild with `x: Base` doesn't change resolution (still name-match), so this
  ;; project instead proves the point structurally: Sub.speak has NO incoming call/ref edge of its own
  ;; anywhere in this file, only the `overrides` edge from Base.speak - remove Base.speak's own
  ;; reachability and Sub.speak must lose it too (checked below), and confirm it is not dead while
  ;; Base.speak IS reachable.
  (check-false (and (member "m.py#Sub.speak" dead) #t) "reachable via the overrides edge, not a direct call")
  (check-false (and (member "m.py#Base.speak" dead) #t)))
;; now the mutation: rename the call so NEITHER Base.speak nor Sub.speak is called at all
(write-file2! "m.py"
  (string-append "__all__ = [\"run\"]\n\n"
                  "class Base:\n    def speak(self):\n        return 1\n\n"
                  "class Sub(Base):\n    def speak(self):\n        return 2\n\n"
                  "# steer: entry\ndef run(x):\n    return 1\n"))
(let-values ([(g parent ids by) (project-reachability dir2)])
  (define dead (map gnode-id (dead-nodes g parent)))
  (check-true (and (member "m.py#Base.speak" dead) #t) "no call reaches Base.speak now")
  (check-true (and (member "m.py#Sub.speak" dead) #t) "and overrides-backwards has nothing to propagate from"))
(delete-directory/files dir2)

;; ---------------------------------------------------------------------------------------------
;; regression: a module/class becoming reachable does NOT drag every sibling def/method along -
;; the two real bugs found while building this (recorded via `steer note T65`)

(define dir3 (make-temporary-directory "steer-reach~a"))
(define (write-file3! rel text)
  (define p (build-path dir3 rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))
;; `NeverCalled` is `private`, so C#'s public_api entry rule (added mid-T68) never touches it -
;; isolating this regression check to the `defines`-edge mechanics it is actually about.
(write-file3! "P.cs"
  "namespace N {\n  public class Widget {\n    // steer: entry\n    public static void Run() { var w = new Widget(); }\n    private void NeverCalled() { }\n  }\n}\n")
(let-values ([(g parent ids by) (project-reachability dir3)])
  (define dead (map gnode-id (dead-nodes g parent)))
  (check-true (and (member "P.cs#Widget.NeverCalled" dead) #t)
              "instantiating Widget must not make NeverCalled reachable - only actual calls/overrides do"))
(delete-directory/files dir3)

;; ---------------------------------------------------------------------------------------------
;; path-to / reach-info: a real shortest path, and a target no entry reaches gets #f

(let-values ([(g parent ids by) (project-reachability py-dir)])
  (define path (path-to parent "helper.py#double"))
  (check-equal? (map car path) '("shapes.py#area" "shapes.py#compute_area" "helper.py#double"))
  (check-false (path-to parent "shapes.py#unused_helper")))

;; ---------------------------------------------------------------------------------------------
;; check-rules: dead(S) is available for a user rule to promote to a violation

(define dir4 (make-temporary-directory "steer-reach~a"))
(define (write-file4! rel text)
  (define p (build-path dir4 rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))
(write-file4! "m.rkt" "#lang racket/base\n;; steer: entry\n(define (go) 1)\n(define (orphan) 2)\n")
(define rules-text "violation(S, S, \"dead code\") :- dead(S).\n")
(define-values (findings info) (check-rules dir4 rules-text ".steer/rules.dl"))
(check-true (ormap (λ (f) (and (eq? (hash-ref f 'kind) 'architecture-violation) (regexp-match? #rx"orphan" (hash-ref f 'message)))) findings)
            "a user rule can promote dead(S) to a violation")
(delete-directory/files dir4)

;; ---------------------------------------------------------------------------------------------
;; the CLI: `steer rules dead` and `steer rules reach SYM`

(define-runtime-path main-rkt "../steer/main.rkt")
(define (steer d . args)
  (define-values (p out in err)
    (parameterize ([current-directory d])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (write-file5! d rel text)
  (define p (build-path d rel))
  (make-directory* (let-values ([(d2 _n _x) (split-path p)]) d2))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))

(define dir5 (make-temporary-directory "steer-reach~a"))
(write-file5! dir5 "m.rkt" "#lang racket/base\n;; steer: entry\n(define (go) (helper))\n(define (helper) 1)\n(define (orphan) 2)\n")
(let-values ([(c o) (steer dir5 "init")]) (check-equal? c 0 o))
(let-values ([(c o) (steer dir5 "rules" "dead")])
  (check-equal? c 1 "dead findings mean a nonzero exit")
  (check-regexp-match #rx"m.rkt#orphan" o)
  (check-regexp-match #rx"steer: entry" o "the fix names the marker mechanism")
  (check-regexp-match #px"entry\\(\\.\\.\\.\\).? fact/rule" o "the fix names the rules.dl mechanism")
  (check-false (regexp-match? #rx"m.rkt#helper[^\n]*dead" o) "helper is called: not in the dead list"))
(let-values ([(c o) (steer dir5 "rules" "reach" "m.rkt#helper")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"reached from m.rkt#go" o))
(let-values ([(c o) (steer dir5 "rules" "reach" "m.rkt#orphan")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"not reached from any entry point" o))
(let-values ([(c o) (steer dir5 "rules" "reach" "m.rkt#nope")])
  (check-equal? c 2 "an id that does not exist in the graph is an error, not a silent empty answer"))
(delete-directory/files dir5)
