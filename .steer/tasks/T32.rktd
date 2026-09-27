((id "T32")
 (title "M1 · criteria parser and `steer spec` ship")
 (status done)
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
 (updated "2026-09-27T04:22:13Z")
 (log
  (((agent "g2-m1")
    (kind note)
    (seq 162)
    (text
     "Milestone M1 gate: tests/spec-cli-test.rkt drives the CLI as an agent (draft with vague criteria -> located refusals with fixes -> apply fixes -> passes -> render -> re-check), run against source and against the compiled binary. It found a real hole: a trailing 'WHEN ...' after the response parsed as ordinary words and passed; now the misplaced-condition lint rule refuses it (comma or ALL-CAPS keyword inside the response). Totals: 291 spec checks; make test 621; make test-bin 58.")
    (ts "2026-09-27T04:21:50Z"))
   ((agent "g2-m1")
    (checks
     (((cmd "raco test tests/spec-cli-test.rkt") (secs 9.7))
      ((cmd "make build && STEER_BIN=$PWD/build/steer raco test tests/spec-cli-test.rkt")
       (secs 12.9))))
    (kind done)
    (seq 163)
    (ts "2026-09-27T04:22:13Z")
    (verified #t)))))
