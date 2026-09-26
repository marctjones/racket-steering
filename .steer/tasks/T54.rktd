((id "T54")
 (title
  "Static Python API lock: __all__, signatures and class members via ast; diff classifies breaks")
 (status open)
 (priority 2)
 (goal
  "`steer api snapshot pkg/mod.py` records public names with ast.unparse signatures without importing the module; diff marks removed names or parameters and new required parameters breaking, added optional parameters compatible, annotation or default changes to review.")
 (after ("T49"))
 (checks ("raco test tests/py-api-test.rkt"))
 (anchors
  (((hash "b1c07ffcfca5") (ref "steer/api.rkt#api-diff"))
   ((hash "fae0beb3ca1f") (ref "steer/api.rkt#read-lock"))))
 (touches ())
 (tags ("cross-language"))
 (claimed-by #f)
 (github ((digest "626595bd66c1") (kind issue) (number 23)))
 (created "2026-09-26T08:04:49Z")
 (updated "2026-09-26T08:05:21Z")
 (log ()))
