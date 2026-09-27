((id "T58")
 (title "Consolidate anchors and gates into one per-language record in lang.rkt")
 (status done)
 (priority 1)
 (goal
  "anchors.rkt's racket/python/csharp conds and lang.rkt's gates become one `lang` struct (check, find-anchor, list-names, extract, resolve-import, entry-prelude, implicit-names, dynamic-calls?) dispatched by extension; every XL1 anchor and syntax test stays green; a language with a missing slot is reported `skipped`, never wrong. Depends on PR #27 (XL1) merging first: steer/lang.rkt does not exist on main yet, so its anchor below is anchor-pending until then, by design.")
 (after ())
 (checks
  ("raco test tests/lang-registry-test.rkt tests/lang-route-test.rkt tests/py-anchors-test.rkt tests/cs-anchors-test.rkt"))
 (anchors (((hash "ccfae7ec8c28") (ref "steer/anchors.rkt#resolve-anchor"))))
 (touches ())
 (tags ("graph"))
 (claimed-by "xl3")
 (created "2026-09-27T08:50:21Z")
 (updated "2026-09-27T09:27:10Z")
 (log
  (((agent "xl3")
    (checks
     (((cmd
        "raco test tests/lang-registry-test.rkt tests/lang-route-test.rkt tests/py-anchors-test.rkt tests/cs-anchors-test.rkt")
       (secs 3.7))))
    (kind done)
    (seq 205)
    (ts "2026-09-27T09:27:10Z")
    (verified #t)))))
