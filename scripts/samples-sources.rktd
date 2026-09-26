;; Pinned sources for sample data (task T8). The data itself is never committed: `make samples`
;; fetches into samples/ (gitignored). Licenses are recorded as GitHub reports them; nothing here is
;; redistributed.
;;
;; (github owner/repo commit license use)
;; (hf dataset config split expected-rows license use)
;; (installation which use)           ; files from the local Racket installation, referenced in place
(
 (github "exercism/racket" "3e7dda0b719d733bf8553e2e11079f567227879f" "MIT"
         "87 practice exercises with tests and reference solutions: T1/T2 tasks, T3 repair mutants")
 (github "jackfirth/rebellion" "8f3fc46740918205c0a293ad58dd9a8094ef3f51" "Apache-2.0"
         "idiomatic modern library code: corpus for syntax/dup/api robustness")
 (github "Bogdanp/koyo" "61d245ce1dacca9d73947d9230a95e747b7bdb94" "unknown"
         "web framework: corpus")
 (github "emina/rosette" "373c8c35e4a7667f38fce10cf0b74ae17de07f1d" "unknown (BSD-2 per repo LICENSE)"
         "solver-aided language: macro-heavy corpus")
 (github "sorawee/fmt" "4e1ed68e596e656960b44a8244bb33eb4e65ec64" "unknown"
         "code formatter: corpus")
 (hf "nuprl/MultiPL-E" "humaneval-rkt" "test" 161 "MIT" "HumanEval translated to Racket: T1 tasks")
 (hf "nuprl/MultiPL-E" "mbpp-rkt" "test" 397 "MIT" "MBPP translated to Racket: T1 tasks")
 (installation "collects" "Racket's own core library: corpus (read in place)")
 (installation "pkgs" "the installation's packages: corpus (read in place)")
)
