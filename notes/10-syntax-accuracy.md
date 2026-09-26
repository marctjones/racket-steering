# 10 · `steer syntax`: false positives and fix accuracy (task T22)

Measured 2026-09-26 on Racket 9.3 CS. Tags as in note 01. Reproduce: `make samples`, then
`racket scripts/corpus-gate.rkt` and `racket scripts/syntax-accuracy.rkt 400 <seed>`.

## Question

When an agent breaks the parentheses of a Racket file, does `steer syntax` (and the PostToolUse hook that
runs it) point at the right place, and is its suggested edit safe to apply? A tool that misleads is worse
than none (note 05), so false positives on valid code are measured first.

## Corpus

6,291 `.rkt` files: the local installation's collects and packages (read in place; all of them compile),
plus five pinned GitHub repositories (`scripts/samples-sources.rktd`). None of it is committed.

## 1. False positives on valid code [tested]

| version | files | ok | skipped (not s-expr) | false positives | crashes | anchors resolving to own hash |
|---|---|---|---|---|---|---|
| v0.1.0 | 6,291 | 6,171 | 88 | 32 (0.5 %) | 2 | 56,357 / 56,357 |
| now | 6,291 | 6,185 | 106 | **0** | **0** | 56,494 / 56,494 |

The 32 false positives came from five header forms the first slice did not know: a `#!/usr/bin/env racket`
line before `#lang` (9), DrRacket teaching-language `#reader(lib "htdp-…")` headers (8), `#reader
scribble/reader` (5), WXME binary files (4), and `#lang racklog` (7, Prolog syntax). The two crashes were
Rosette's `||` operator, which Racket reads as the empty symbol and which broke anchor names. Each case is
now a regression test in `tests/unit-test.rkt`. Explicit `(module name lang …)` files no longer get a
`no-lang` warning.

## 2. Fix accuracy on seeded errors [tested]

Method: 400 corpus files per run, sampled by a seeded hash; in each file one mutation of each kind at a
seeded position: **delete** one `)`/`]`, **duplicate** one (extra closer), **swap** `)`↔`]` (mismatch). For
each of the 1,200 trials: did steer report an error, did it offer a machine-applicable edit, does applying
that single edit give back **exactly** the original datum, does the file at least read, and how far is the
reported line from the mutated one. The baseline is the line of the raw reader error, which is what an agent
sees without steer.

v0.1.0 heuristic (reset at column-0 forms), seed `t22`:

| mutation | exact repair | readable after edit | steer line exact | reader line exact |
|---|---|---|---|---|
| delete closer | 38 % | 73 % | 55 % | 39 % |
| extra closer | 39 % | 71 % | 66 % | 66 % |
| swap closer | 98 % | 98 % | 98 % | 100 % |
| all | 58 % | 81 % | 73 % | 68 % |

Indentation-guided search with verification (current), seed `t22` / fresh seed `fresh-seed-2`:

| mutation | exact repair | readable after edit | steer line exact | steer ±2 lines | reader line exact |
|---|---|---|---|---|---|
| delete closer | 85 % / 82 % | 99 % / 99 % | 96 % / 97 % | 97 % / 98 % | 39 % / 40 % |
| extra closer | 76 % / 78 % | 84 % / 86 % | 92 % / 96 % | 95 % / 96 % | 66 % / 62 % |
| swap closer | 98 % / 99 % | 98 % / 99 % | 98 % / 99 % | 98 % / 99 % | 100 % / 100 % |
| **all** | **86 % / 86 %** | **94 % / 95 %** | **96 % / 97 %** | 97 % / 98 % | 68 % / 67 % |

Every trial was detected and got an edit in both versions. The design was developed while looking at the
`t22` numbers; the fresh seed was run once afterwards and agrees within noise.

How it works (`steer/syntax-check.rkt`): lex brackets and each line's first code column; propose repairs
where the indentation contradicts the brackets ((a) a line starts at or left of a still-open opener's
column: close it at the end of the previous code line; (b) a line is indented inside a form that the
previous line closed: that closer is extra) plus the reader's own trouble spots; apply each candidate,
keep those after which the file reads, prefer the one leaving the fewest indentation violations. The
finding says `verified` when that check passed, and `steer syntax --fix` applies only verified edits.

Cost: a clean 2,800-line file checks in about 20 ms; the same file with a deleted closer takes 0.3-0.6 s
while candidates are verified (measured under heavy machine load).

## 3. What this does and does not show

- Seeded one-character errors in **well-formatted** code are the easy case. Model-written code with a
  broken paren may also be mis-indented, which weakens rule (a)/(b). The next number to get is the same
  table on real model failures from the E1 logger (T7), not on seeded ones.
- "Exact repair" is strict: a different edit that yields an equivalent program counts as a miss, so the true
  rate is at least this high. Conversely, ~9 % of verified edits read fine but nest differently than the
  original, which is why the skill and `--fix` output say to rerun the tests.
- Extra closers are the hardest case (76-78 %): when several closers sit together at a line end, deleting any
  of them reads fine, and indentation cannot always tell which one was intended.
- Nothing here measures whether agents *fix faster* with the hint. That is note 04's C1 vs C2 comparison.
