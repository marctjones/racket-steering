# 06 · Continuity, planning and drift tools

Legend as in note 01: **[known]** documented behaviour, **[tested]** checked on this machine (Racket 9.3 CS,
2026-09-26), **[hyp]** usefulness hypothesis to be measured by note 04.

This note widens the scope from "check the Racket code an agent writes" to "keep an agent's *work* coherent across
time": plans with dependencies, fresh or reset contexts resuming work cheaply, and detecting when code drifts from a
plan, a public API or a module's declared purpose. Most of these tools help with *any* target language; that tension
with note 05's non-goal is discussed at the end.

## 1. Division of labour

| a deterministic tool is better at | the model is better at |
|---|---|
| bookkeeping across time (tasks, statuses, decisions, cursors) | intent, decomposition of a goal into steps |
| exhaustive enumeration (all callers, all exports, all transitive dependents) | naming, explaining, summarising |
| exact comparison (API before/after, normalised code shape) | fuzzy similarity ("these do the same job") |
| gates: tests pass, graph acyclic, change stayed inside declared scope | judging whether a change was *intended* |
| budget arithmetic: what fits in N tokens, what is ready, the critical path | choosing what to do when the plan is wrong |

**Operating rule:** the model records its judgments as *structured declarations* ("task T3 adds `export-csv` with this
signature and touches only `report/`"); the tool checks those declarations against reality later and forever. The
model decides once; the tool remembers and enforces. [hyp] This is what lets a fresh, small context continue a plan.

## 2. What is specifically Racket about this (and what is not)

A tracker or planner can be written in any language. Racket's real edges here are narrower:

1. **Checked plan/issue files.** Plans are s-expressions read with `read-syntax`, so every validation error has a
   line and column, and the error text is ours to design for an LLM. [known] A full `#lang plan` needs an installed
   collection to resolve, which fights a standalone executable; v1 validates `(task ...)` forms directly instead.
2. **`#lang datalog` for closure queries and rules over facts.** [tested] Transitive dependencies and layering rules
   work (`dep(t4, X)?` → t3, t2, t1; a ui→db layering violation is found). **No negation**: `not p(X)`, `\+ p(X)`
   and `~p(X)` all fail to parse, so "ready = open and no undone dependency" is plain Racket, not Datalog. [tested]
3. **Exact facts when the target code is Racket:** `module->exports`, `procedure-arity`, contracts via
   `value-contract`, fully expanded code for alpha-equivalence. [known; probe before relying on each]

For non-Racket targets the facts would come from tree-sitter or an LSP, and Racket is only the rule engine: a weaker
case.

## 3. Agent-friendly tracker (F1) and anchors (F2)

Design points that separate it from a TODO file:

- **Executable acceptance criteria.** `done` runs the task's `done-when` commands and refuses on failure. A task
  without checks can only be closed with an explicit, logged `--unverified REASON`. Aim: stop "claimed done, isn't".
- **Symbol anchors with content hashes.** A task points at `file.rkt#name`, not at line numbers. The hash is taken
  over the *datum* of the definition, so reformatting and comments do not make it stale. A changed hash means "the
  plan may be stale": this is plan-drift detection. Anchors to symbols that do not exist yet are allowed (tasks
  that create them).
- **Budgeted resume packets.** `resume` / `next` return the goal, checks, where anchors are *now*, recent decisions
  and the last checkpoint, capped at ~2 KB (note 03 rule); short stable ids give progressive disclosure.
- **Event cursor.** Every mutation appends to an event log with a sequence number; `resume` prints the cursor and
  `since N` reports what other agents did meanwhile.
- **Commands, not file edits.** The model talks to the store through the CLI; it never edits the s-expression files
  (paren risk, same reason as A1's `apply-edit`). Files stay git-diffable for humans: one file per task,
  write-then-rename, a lock around mutations.
- **Validated checkpoints.** `checkpoint` requires "did" and "next" fields. With them, "checkpoint, clear context,
  resume" becomes a supported workflow rather than a hope. [hyp]
- **Checked bulk plans.** `import` reads a file (or stdin) of `(task ...)` forms with local labels, validates
  references, cycles and field types against the existing store, and applies all or nothing.

## 4. Drift (F3, F4, F6)

| drift | tool | model |
|---|---|---|
| public API (F3) | snapshot exports + arity + contracts to `api.lock`; diff; classify breaking / compatible (cf. cargo-semver-checks, japicmp) | whether a break is intended |
| architecture (F4) | Datalog rules over the require graph (cf. ArchUnit) | whether a rule should change |
| purpose (F6) | flag new exports/requires not covered by a module's declared responsibilities | whether the code belongs, or the declaration needs updating |
| plan (F2) | declared `touches`/anchors vs actual change; stale anchor hashes | update the plan or revert |

Purpose drift is mostly judgment; the tool's job is only to notice *uncovered change* and route it to the model.

## 5. Duplicate code (F5)

- **Same shape (tool):** hash subtrees after consistent renaming of locally bound identifiers; report maximal clone
  groups with locations. For Racket, fully expanded code would also see through different macros (later).
- **Near-miss / cross-language (tool):** token k-grams + winnowing (MOSS technique). Later.
- **Same behaviour, different code:** model or embeddings *propose* pairs; A6 (differential testing, bounded Rosette)
  *verifies*.
- **Prevention:** search by signature/contract before writing a helper (an extension of B1).

## 6. Prior art (from memory; verify before relying on it)

beads (Steve Yegge's git-backed issue tracker for agents with dependencies and a ready query), claude-task-master,
Claude Code's own task list. Differentiators to test rather than assume: executable acceptance, hashed symbol
anchors, rule checking, a checked plan format with located errors.

## 7. Scope decision

Note 05 lists "steering non-Racket languages" as a non-goal. Proposal: keep it for *code checking* tools (A-series),
but except the F-series tracker/continuity tools, which are language-independent in value and use Racket for their
own format, checks and rules. Measure with note 04's T5 (agentic multi-step) plus a forced context reset mid-task.
