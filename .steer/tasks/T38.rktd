((id "T38")
 (title "Assemble a criteria corpus with labels")
 (status open)
 (priority 2)
 (goal
  "samples/eval/criteria/ (gitignored like the rest of samples/): this repo's own task goals, exercism task statements from the non-holdout split, and 30 hand-written requirements; each labelled parseable-as-written / needs-rewrite / not-a-requirement, plus the rewritten controlled form. scripts/criteria-eval.rkt builds it and reports status.")
 (after ("T31"))
 (checks
  ("test $(ls samples/eval/criteria/*.txt 2>/dev/null | wc -l) -ge 60"
   "racket scripts/criteria-eval.rkt status"))
 (anchors ())
 (touches ())
 (tags ("regular-grammar"))
 (claimed-by #f)
 (github ((digest "32de3256bef0") (kind issue) (number 8)))
 (created "2026-09-26T07:48:37Z")
 (updated "2026-09-26T08:03:43Z")
 (log ()))
