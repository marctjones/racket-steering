((id "T36")
 (title "Teach the criteria form in the steer-tasks skill within a token budget")
 (status open)
 (priority 2)
 (goal
  "skills/steer-tasks/SKILL.md gains a `## Criteria` section with the five templates and three examples so a model writes criteria instead of prose goals; the section stays at or under 300 words (about 400 tokens) because the skill is in every session's prefix.")
 (after ("T33"))
 (checks
  ("grep -q '^## Criteria' skills/steer-tasks/SKILL.md"
   "grep -q 'SHALL' skills/steer-tasks/SKILL.md"
   "test $(sed -n '/^## Criteria/,/^## /p' skills/steer-tasks/SKILL.md | wc -w) -le 300"))
 (anchors ())
 (touches ())
 (tags ("regular-grammar"))
 (claimed-by #f)
 (github ((digest "d39df8a9c6fb") (kind issue) (number 7)))
 (created "2026-09-26T07:48:37Z")
 (updated "2026-09-26T08:03:43Z")
 (log ()))
