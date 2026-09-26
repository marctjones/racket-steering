((id "T32")
 (title "M1 · criteria parser and `steer spec` ship")
 (status open)
 (priority 1)
 (goal
  "Milestone: an agent can run `steer spec check -` on a block of criteria and get either the structured form or located, fixable errors, from the compiled binary. Proven by an end-to-end CLI test that drives racket steer/main.rkt (and build/steer with STEER_BIN) like tests/cli-test.rkt does.")
 (after ("T28" "T29" "T30" "T31"))
 (checks
  ("raco test tests/spec-cli-test.rkt"
   "make build && STEER_BIN=$PWD/build/steer raco test tests/spec-cli-test.rkt"))
 (anchors ())
 (touches ())
 (tags ("regular-grammar" "milestone"))
 (claimed-by #f)
 (github ((digest "afbd6f2d2540") (kind milestone) (number 1)))
 (created "2026-09-26T07:48:36Z")
 (updated "2026-09-26T08:03:23Z")
 (log ()))
