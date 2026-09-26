# 04 · Evaluation plan

Purpose: find out whether steering tools raise the rate of *correct* Racket (or DSL) code from LLMs, especially
smaller models, and which tools account for the gain. Everything else in this repo is speculation until this runs.

## Questions

1. Does tool feedback raise pass rate vs. no tools? By how much per tool?
2. Does it narrow the gap between a small model and a large model?
3. Cost: extra tokens, turns, wall time per solved task.
4. Which failure classes remain (the taxonomy) and which tool would address them?
5. Is a restricted DSL (C1/C3) easier for models than free-form Racket, net of the prompt cost of teaching it?

## Task sets (build or collect)

- **T1 Racket exercises:** ~100-200 small tasks (list processing, strings, structs, match, macros, contracts,
  file IO, `for` loops) with hidden unit tests. Sources: HtDP-style problems, Rosetta-code Racket, Exercism Racket
  track (check licence), our own. Keep a hold-out set never used for tuning tools/prompts.
- **T2 Library use:** tasks needing real library calls (`racket/list`, `racket/string`, `db`, `plot`, `json`, `web-server`)
  where hallucinated APIs are the main failure. Directly tests B1/A2.
- **T3 Repair:** broken Racket programs (seeded unbound names, arity errors, paren mismatches, wrong idioms) with a
  test that should pass after repair.
- **T4 Data-processing:** CSV/JSON transformation tasks with a checker on output; compare free-form scripts vs the
  pipeline DSL (C3).
- **T5 Agentic multi-step:** tasks with tool use and side effects in a scratch dir; measure policy violations
  prevented by C1/C2 and task success.

## Conditions (each task x model x condition, several seeds)

- C0 baseline: model alone, single attempt.
- C1 retry with raw error output (what agents do today).
- C2 retry with our structured feedback (A1+A2+A3).
- C3 C2 + doc lookup (B1) available as a tool.
- C4 C3 + auto-injected hooks (feedback without asking).
- C5 restricted DSL variants where applicable.
- Models: at least one small, one medium, one large; same prompts across conditions; temperature fixed and reported.
- Budget cap per attempt (turns, tokens) identical across conditions.

## Metrics

- Pass rate at k attempts (pass@1, pass@3 with feedback loop), with confidence intervals (bootstrap).
- Turns/tokens/time to first correct solution; cost per solved task.
- Failure taxonomy counts per condition (unbound id, arity, syntax, idiom, semantic, timeout, tool misuse).
- Tool-call rate (does the model call optional tools? compare C3 vs C4).
- False positive rate of each tool (feedback that misleads); track separately, this can make things worse.
- For C5: rate of policy violations blocked, and rate of valid tasks wrongly rejected.

## Method notes

- Randomize/interleave conditions to avoid drift in model versions or rate limits; record model ids and dates.
- Report medians and intervals; do not claim gains smaller than the run-to-run noise.
- Keep a frozen tool version per run; log all tool inputs/outputs (E1) for replay (E2).
- Held-out test set is scored once per tool generation.
- Beware contamination: some public exercises are in training data; include newly written tasks.

## Decision rules (set in advance)

- Keep a tool if it improves pass@3 by a margin above noise on T1-T3 without raising false-positive harm.
- Drop or redesign a tool whose feedback is followed but does not raise pass rate.
- Prioritise the next tool by the largest remaining failure class in the taxonomy.
