---
name: steer-tasks
description: Plan and continue multi-step coding work with the `steer` task tracker - tasks with dependencies, executable acceptance checks, checkpoints and resume packets stored in .steer/. Use when the repo has a .steer/ directory, when a SessionStart message shows "steer resume", when breaking a feature into steps, when context is getting long and progress must survive a /clear or compaction, or when finishing a task (steer done runs the checks).
allowed-tools: Bash(steer *)
---

# steer task tracker

`steer` keeps the plan outside your context. You read small packets instead of files, and the tool,
not you, decides when a task is done: `steer done` runs the task's checks and refuses on failure.
Never edit `.steer/` by hand; always go through commands.

## Session loop

1. **Orient:** `steer resume` (it may already be in context from the SessionStart hook). It shows your
   active task with its last checkpoint, what is ready, stale anchors, and an event cursor `#N`.
2. **Pick:** `steer next --claim`. Work only on the claimed task.
3. **Record decisions** as you make them: `steer note T3 "chose X because Y"`.
4. **Finish:** `steer done T3`. If a check fails, read the tail it prints, fix, run again.
   Use `--unverified "reason"` only when no command could prove the result, and say why.
5. **Before context runs out, or when stopping:**
   `steer checkpoint T3 --did "what is finished and verified" --next "the exact next action"`.
   After that it is safe to /clear; the next session starts from `steer resume`.
6. Back after a while, or other agents are active: `steer since N` (N = cursor from resume).

## Planning

Make each task one sitting of work with a check that proves it. Import a whole plan at once; it is
validated (unknown refs, typos, cycles) before anything is written:

```
steer import - <<'PLAN'
(task "Parse CSV rows" #:id parse
  #:goal "rows → list of hashes; header row gives keys"
  #:check "raco test tests/parse-test.rkt"
  #:anchor "src/parse.rkt#parse-rows")          ; code this task creates or depends on
(task "Export report" #:after (parse T4) #:check "raco test tests/export-test.rkt")
PLAN
steer graph      # layers, critical path, problems
```

Good checks are fast and specific: `raco test f.rkt`, `pytest tests/test_x.py::test_y`,
`npm test -- -t name`, `grep -q 'fn name' src/x.rs`, `steer api diff`, `steer syntax f.rkt`.
Change a plan with `steer edit T5 --add-check CMD --add-after T2 --goal "..."`;
abandon with `steer drop T5 --reason "..."`.

## Working on a branch

Task ids are sequential, so two branches that each add a task both create the next id. Before
merging a branch that added tasks, run `steer doctor --against main --fix` on it: it renumbers your
colliding tasks (and the `--after` references to them) and resequences the merged event log.
`steer doctor` alone checks the store: unreadable or conflicted task files, dangling dependencies,
cycles, and tasks claimed for more than a week.

## Stale anchors

An anchor (`file#name`) remembers a hash of that definition when the plan was written. A
`stale-anchor` warning means the code changed since then: re-read that definition, adjust the task
(`steer edit`), then `steer refresh T5`.

## Output

Text by default (short, located, with a fix after `→`). Add `--json` to parse. Exit codes: 0 ok,
1 findings or refused, 2 usage error (read the hint), 3 internal. With several agents, pass
`--agent NAME` (or set STEER_AGENT) so claims stay separate. `steer help CMD` shows usage.
