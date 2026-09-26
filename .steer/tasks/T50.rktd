((id "T50")
 (title
  "C# anchors that see generics, partial classes, properties, attributes, Allman bodies and overloads")
 (status open)
 (priority 2)
 (goal
  "Replace the indentation block with a brace-balanced block from the declaration line, include leading attributes, accept `Name<T>(`, properties and `=>` members, `partial`/file-scoped namespaces, and disambiguate overloads as Name/arity or Name(type,type); measured against the GuardClauses shapes from note 12.")
 (after ("T46"))
 (checks ("raco test tests/cs-anchors-test.rkt"))
 (anchors
  (((hash "5e66289ce955") (ref "steer/anchors.rkt#heuristic"))
   ((hash "a85b0b46f7b3") (ref "steer/anchors.rkt#block-end"))))
 (touches ())
 (tags ("cross-language"))
 (claimed-by #f)
 (github ((digest "f2af74a717d4") (kind issue) (number 20)))
 (created "2026-09-26T08:04:49Z")
 (updated "2026-09-26T08:05:18Z")
 (log ()))
