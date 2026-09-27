((id "T76")
 (title
  "Vision F8 · a project-level goals record, separate from and complementary to the task tracker")
 (status open)
 (priority 1)
 (goal
  "End to end: steer init leaves vision empty and steer resume says so plainly; steer vision set (gated per vision-gate's convention) writes the north star and a ranked priority with a rationale; steer vision show and the resume packet both reflect it; every write appears in steer since N's event stream with its diff. Verified this is additive, not a fork of the existing continuity model: F1 (tasks)/F2 (anchors) are completely unaffected by a project with no vision set at all. Catalog updated: notes/02-tool-catalog.md gains F8 alongside F1-F7; notes/06-continuity-and-drift.md gets a short section placing vision relative to stale/rules (drift in CODE vs PLAN) as the plan's own GOALS drifting without a decision, the one gap those two do not cover.")
 (after ("T71" "T72" "T73" "T74"))
 (checks ("raco test tests/vision-integration-test.rkt"))
 (anchors ())
 (touches ())
 (tags ("vision" "milestone"))
 (claimed-by #f)
 (github ((digest "0fed19b1d704") (kind milestone) (number 6)))
 (created "2026-09-27T18:12:49Z")
 (updated "2026-09-27T18:14:03Z")
 (log
  (((agent "claude")
    (kind note)
    (seq 226)
    (text
     "Design proposed by a peer session (Racket Skia backend, working on a local embedded coding LLM for a Racket IDE) via cross-session message, 2026-09-27: they identified this as the one continuity gap the existing model doesn't cover - stale/rules catch code drifting from a plan, nothing catches the plan's own goals drifting without anyone deciding they should. Catalog slot F8, alongside F1-F7 in notes/02-tool-catalog.md.")
    (ts "2026-09-27T18:13:50Z")))))
