# 02 · Tool catalog

Each entry: purpose, input, output (what the model sees), build effort (S = days, M = 1-3 weeks, L = a month+),
value guess (H/M/L, all guesses), risks. Priority 1 tools form the suggested first slice.

Common feedback rule: outputs are **short, stable JSON with source locations and a suggested fix**, never raw stack
traces. The model should be able to act on the first error alone.

## A. Check what the agent produced

### A1 · Structural validator (priority 1, S, value H)
- In: code text (may include markdown fences/prose). Out: `{ok, forms, errors:[{line,col,kind,message}]}`.
- Extracts code, reads all forms, reports unbalanced/extra parentheses with the *likely* fix location (indentation
  vs paren mismatch heuristic), missing `#lang`, reader errors.
- Also offers `apply-edit`: structural edits by path (`replace form 3.2 with ...`), so the model never re-types a
  whole file.

### A2 · Binding and arity checker (priority 1, M, value H)
- In: module source + project path. Out: unbound identifiers with nearest-name suggestions, imports that do not
  exist, duplicate definitions, wrong arity where statically known, unused requires.
- Uses the expander inside the sandbox. Nearest names come from exports of required modules (edit distance +
  doc keyword match).
- Risk: expansion executes macros; must be sandboxed and time-limited.

### A3 · Blame-returning check loop (priority 1, M, value H)
- In: code + optional spec (contracts, examples, tests). Out: result of `raco make` + `raco test` + contract run as
  a single report: first failure, blame party, offending value, minimal repro.
- Runs in sandbox with time/memory limit. Returns at most N failures, ordered by likely root cause.

### A4 · Property tester (M, value M)
- In: function + contract/type or generator hints. Out: shrunk counterexample or "passed N cases, seed S".
- Generators derived from contracts where possible.

### A5 · Typed Racket gate (M, value M/unknown)
- Try the code under `#lang typed/racket` (or add annotations proposed by the model) and return type errors in
  normalized form. Test whether models do better *with* the type oracle than without.

### A6 · Differential / round-trip checker (M, value M)
- In: original + rewrite (refactor, optimization, translation). Out: inputs where they differ, or equivalence
  evidence (random + optional Rosette proof within bounds).

### A7 · Lint and idiom checker (S, value M)
- Detect non-Racket idioms models often produce: Scheme/Common Lisp names (`define-macro`, `setq`, `mapcar`,
  `1+`), wrong `for` clause shapes, `set!` misuse, accidental `#lang scheme`, `car`/`cdr` loops where `for`/`match`
  are idiomatic. Rule list grows from observed model failures (see 04: failure taxonomy).

## B. Tell the agent what exists

### B1 · Documentation and signature lookup (priority 1, S-M, value H)
- Queries: `sig <id>`, `search <words>`, `examples <id>`, `exports <module>`, `similar <id>`.
- Out: signature with contract, one-line description, one example, "see also". Built from the doc index and
  module exports; cached.
- Guard against hallucinated ids: `exists? <id> in <module>` is O(1) and cheap to call every time.

### B2 · Project map (S-M, value M/H)
- Index of modules, provides, requires, definitions with line ranges, call graph, test coverage per function.
- Lets the agent request "the function and its callers" instead of reading whole files (saves context).

### B3 · Error explainer (S, value M)
- Maps common Racket error messages to causes and fixes, with examples from our own failure logs.

### B4 · Context assembler (M, value M)
- Given a task and file, deterministically builds the prompt context: relevant definitions, their contracts, related
  tests, recent failure output, style rules. Aim: reduce reliance on model memory.

## C. Constrain what the agent may do

### C1 · Capability language for tools (M, value H for agentic use)
- A `#lang` where tool permissions are declared: name, argument contracts, preconditions, effects, budget, audit
  log fields. Calls are checked *before* execution; violations return which rule failed.
- Also yields an audit trail and a policy that can be diffed and reviewed by humans.

### C2 · Plan language and plan checker (M, value M/H)
- Agent emits a plan in a restricted language (steps, inputs, outputs, effects). Checker verifies dataflow (each
  step's inputs exist), effect policy (no writes outside dir), budget, and that reads happen before dependent
  writes. Only then does an executor run it, step by step, with per-step checks.

### C3 · Data-pipeline DSL with schema checking (M-L, value H for data tasks)
- Typed transformations (parse, filter, join, aggregate, validate) over tables/records. Each step declares input and
  output schema; the checker verifies compositions before touching data; runtime enforces schemas and reports the
  first violating row.
- Dry-run mode on a sample; row-level provenance so the agent can explain outputs.

### C4 · Sandboxed executor (S-M, value H)
- Wraps `racket/sandbox` (and optionally OS-level isolation) with limits and structured results. Foundation for A3,
  A4, C2, C3.

## D. Verify against a specification

### D1 · Rosette spec checker (L, value M, research-y)
- Small spec language ("for all lists xs, (sort xs) is sorted and a permutation of xs") compiled to Rosette
  queries over bounded inputs. Returns counterexample or "verified up to bound k".

### D2 · Sketch completion (L, value M/unknown)
- Model writes code with `??` holes; solver fills them against examples/specs; tool returns the filled code or an
  unsat core the model can read.

## E. Meta tools

### E1 · Failure taxonomy logger (S, value H for everything else)
- Record every model attempt, tool feedback and outcome; classify failures (unbound id, arity, syntax, wrong idiom,
  wrong semantics, timeout). This drives which tools to build next and what lint rules to add. Build first.

### E2 · Replay harness (S-M)
- Replays recorded agent sessions with a modified tool set to measure the tool's effect without new model calls
  where possible.

## F. Continuity and drift (see note 06)

Language-independent in value; the tracker is implemented first as the `steer` CLI (`steer/`, see README).

### F1 · Agent-friendly task tracker (S-M, value H/unknown)
- Store: `.steer/` in the project, one s-expression file per task, append-only event log with sequence numbers,
  lock around mutations. Model uses commands only.
- Commands: `add`, `import` (checked bulk plan, stdin ok), `list`, `ready`, `next`, `claim`, `note`, `checkpoint`,
  `done` (runs acceptance checks, refuses on failure), `resume` (≤2 KB packet), `since N`, `graph`.
- Out: short text by default, `--json` for the note-03 protocol. Stable exit codes for hooks.

### F2 · Symbol anchors and plan-staleness (S, value M/H)
- Anchor `file#name` hashed over the definition's datum (format-insensitive). `stale` lists open tasks whose anchors
  changed since planning; `refresh` re-baselines after review. Non-Racket files: heuristic definition finder.

### F3 · Public API lock and diff (M, value M/H)
- Snapshot exports, arity and contracts of listed Racket modules to `.steer/api.lock` (worker process, time limit);
  `diff` classifies removed/narrowed as breaking, added/widened as compatible, contract text changes as "review".

### F4 · Architecture rules (M, value M)
- Extract require-graph facts; check Datalog rules (layering, allowed callers). Report the violating edge.

### F5 · Clone detection (S-M, value M)
- Racket: subtree hashing after alpha-renaming of local binders, maximal groups only. Later: token winnowing for
  near-miss and non-Racket code; "does something like this exist?" query for a single definition.

### F6 · Purpose manifest (M, value unknown)
- Per-module declared responsibilities/allowed exports and requires; flag uncovered change for model review.

### F7 · Harness integration (S, value H/unknown)
- Claude Code skills shipped inside the binary (`steer skills install`); hook commands: SessionStart runs `resume`,
  PostToolUse runs structural checks on edited `.rkt` files.

### F8 · Vision and priorities record (S, value unknown) — proposed 2026-09-27, tracked as T71-T76
- A north star (rarely changes) plus a ranked priority list, separate from tasks; `steer vision show`/`set`,
  folded into `resume`'s packet the way an active task already is. The gap F1-F7 do not cover: `stale`/F4's rules
  catch CODE drifting from a PLAN; nothing catches the PLAN's own GOALS drifting without anyone deciding they
  should. `set` is a distinctly-named write path specifically so a host's own permission system can gate it on
  human confirmation - steer cannot know who is typing, so that protection has to come from outside the tool.
  Proposed by a peer project's session (a local embedded-LLM Racket IDE, via cross-session message) hitting this
  gap in practice, not from this project's own backlog review.

## Build order suggestion

E1 → A1 → B1 → A2 → A3 (first slice, run evaluation) → C4 → then choose among C1/C3 (agentic) or A4/A6 (quality)
based on measured failures. D-tools last.
