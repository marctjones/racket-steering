((id "T41")
 (title "Apply the note 04 decision rules to the criteria tool")
 (status open)
 (priority 2)
 (goal
  "Keep, redesign or drop, decided by the pre-set rules: keep if wrong-done claims fall by more than run-to-run noise without raising false rejects above the agreed bound. Record the decision in notes/13 and update the status line of note 11.")
 (after ("T40"))
 (checks
  ("grep -q '^## Decision' notes/13-criteria-results.md"
   "grep -qE 'Status.*(kept|redesigned|dropped)' notes/11-regular-grammar-languages.md"))
 (anchors ())
 (touches ())
 (tags ("regular-grammar"))
 (claimed-by #f)
 (github ((digest "f48d3fc5bba5") (kind issue) (number 12)))
 (created "2026-09-26T07:48:37Z")
 (updated "2026-09-26T08:03:46Z")
 (log ()))
