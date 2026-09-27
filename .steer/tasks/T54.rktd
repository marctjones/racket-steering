((id "T54")
 (title
  "Static Python API lock: __all__, signatures and class members via ast; diff classifies breaks")
 (status open)
 (priority 2)
 (goal
  "steer api snapshot pkg/mod.py records public names with ast.unparse signatures without importing the module; diff marks removed names or parameters and new required parameters breaking, added optional parameters compatible, annotation or default changes to review. Its signature format is what shape-lock (T67) reads through the generic graph-ir shape field, so this task's output composes with the same-shaped Racket/C# entry-shape locks rather than being a standalone Python-only lock format.")
 (after ("T49"))
 (checks ("raco test tests/py-api-test.rkt"))
 (anchors
  (((hash "b1c07ffcfca5") (ref "steer/api.rkt#api-diff"))
   ((hash "fae0beb3ca1f") (ref "steer/api.rkt#read-lock"))))
 (touches ())
 (tags ("cross-language"))
 (claimed-by #f)
 (github ((digest "2f9405547e89") (kind issue) (number 23)))
 (created "2026-09-26T08:04:49Z")
 (updated "2026-09-27T08:52:57Z")
 (log
  (((agent "claude")
    (kind note)
    (seq 167)
    (text
     "Reworded (no behavior change) so its output is explicitly framed as feeding shape-lock's (T67) generic entry-shape field rather than a Python-only lock format - part of the XL3 code-graph design's re-scoping of T54-T57.")
    (ts "2026-09-27T08:51:18Z")))))
