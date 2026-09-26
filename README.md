# racket-steering

[![test](https://github.com/marctjones/racket-steering/actions/workflows/test.yml/badge.svg)](https://github.com/marctjones/racket-steering/actions/workflows/test.yml)

Notes and tools for **steering LLM agents with deterministic Racket programs**: validators, checkers,
retrieval, capability languages and verifiers that an agentic system calls while it writes code or processes data.

Status (2026-09-26): notes plus a first working slice, the **`steer` CLI** (below). The tools are built and
tested; whether they *help a model* is still a hypothesis until the evaluation in `notes/04-evaluation.md`
runs. The backlog lives in this repo's own tracker: `steer list` / `steer graph`.

## The idea in one paragraph

An LLM proposes; a deterministic tool disposes. The tool does not need to be smart. It needs to be *exact*, *fast*,
and to return feedback in a form the model can act on (a location, a rule that was broken, a counterexample, the
correct signature). Racket is an unusually good host for such tools because the language is its own AST (code is
data), its expander can answer "what does this identifier mean here?", its contract system attributes blame, its
macro system lets us define small checked languages for agents to write in, and Rosette adds a solver.

## `steer` CLI

One executable, short text output (or `--json` in the note-03 protocol), stable exit codes
(0 ok · 1 findings/refused · 2 usage · 3 internal), about 70 ms warm startup.

| area | commands | catalog |
|---|---|---|
| task tracker | `init add import list show ready next claim release note checkpoint done verify drop reopen edit` | F1 |
| measurement | `failures [--by class\|kind\|tool\|agent]`: every command logs its error/warning findings (locally, gitignored, clipped); the largest class says what to build next | E1 |
| store health | `doctor [--against REF] [--fix]`: corrupt/conflicted files, dangling deps, stale claims, ids another branch uses for a different task (renumbers ours), duplicate event numbers after merges | F1 |
| continuity | `resume` (budgeted packet) · `since N` (event cursor) · `graph` (cycles, layers, critical path) | F1 |
| plan drift | `stale` / `refresh`: symbol anchors (`file#name`) hashed over the datum, so reformatting is not drift | F2 |
| Racket code | `syntax [--fix]` (reader error + a verified, indentation-guided repair) · `dup` (clones modulo renaming) · `api snapshot/diff/show` | A1 F5 F3 |
| architecture | `rules check\|facts\|init`: layering rules as Datalog over the require graph; each violation shows the require path (this repo checks itself: `.steer/rules.dl`) | F4 |
| Racket docs | `doc exists\|sig\|search\|exports`: is this name real, its documented signature and `(require ...)`, nearest racket names for a wrong one | B1 |
| harness | `skills install` · `hook session-start` · `hook post-edit` · `hook config` | F7 |

What makes it agent-friendly: `done` runs the task's checks and refuses on failure; `checkpoint` requires
`--did` and `--next`, so a cleared context can continue from `steer resume`; plans are imported
all-or-nothing with located errors and did-you-mean fixes; every mutation is locked and logged.

### Build and install

Needs Racket 9 (tested on 9.3 CS).

```bash
make          # build/steer (raco exe)
make test     # unit, GitHub-sync and end-to-end CLI tests (163 at v0.2.0)
make test-bin # the end-to-end suite against the compiled binary
make install  # dist/ (self-contained) + symlink in ~/.local/bin (PREFIX=... to change)
```

The distributed binary runs without Racket installed, except `steer api` and `steer doc`, which use the
installed `racket` (to load user modules, or to read its documentation) in a time-limited worker process.

### Sample data (never committed)

Measurement data lives in `samples/` (gitignored) and is rebuilt from pinned sources listed in
`scripts/samples-sources.rktd`: five GitHub repositories at fixed commits, MultiPL-E's Racket HumanEval and
MBPP, and the local Racket installation (read in place).

```bash
make samples           # fetch + build eval sets: T1/T2 tasks and T3 repair mutants, ~19% held out
make samples-validate  # every exercism reference passes its tests, every mutant fails them
racket scripts/corpus-gate.rkt         # false positives / crashes of steer over ~6,300 real files
racket scripts/syntax-accuracy.rkt 400 # seeded paren errors: fix accuracy (results in notes/10)
racket scripts/doc-hallucination.rkt   # wrong identifiers: flagged? right name suggested? (notes/14)
```

Tasks whose check reads `samples/` (e.g. T8) fail on a fresh clone until `make samples` has run.

### Use with Claude Code

```bash
steer init --skills          # .steer/ + .claude/skills/steer-tasks and steer-code
steer hook config            # settings.json snippet: SessionStart resume + PostToolUse syntax check
```

The SessionStart hook also fires after `/clear` and compaction, so a fresh context receives the
resume packet (active task, last checkpoint, what is ready, stale anchors) without reading files.
Skills can also be installed for all projects with `steer skills install --user`.

Plan format (`steer help import`):

```racket
(task "Add CSV export" #:id csv #:after (schema T3)
  #:goal "What and why" #:check "raco test report/export-test.rkt"
  #:anchor "report/export.rkt#export-csv" #:priority 1 #:tag export)
```

## Contents

| path | what it holds |
|---|---|
| `notes/01-capabilities.md` | Racket capabilities that are useful for steering tools, why, and their limits |
| `notes/02-tool-catalog.md` | concrete tool ideas (A-F series), inputs, outputs, feedback format, effort, value guess |
| `notes/03-architecture.md` | how the tools reach an agent (CLI, MCP server, hooks), the shared feedback protocol, sandboxing |
| `notes/04-evaluation.md` | how to test whether any of this helps, benchmarks, metrics, controls |
| `notes/05-prior-art-and-questions.md` | related work to read, open questions, risks, non-goals |
| `notes/06-continuity-and-drift.md` | tracker, anchors, drift and clone tools; tool/model division of labour; tested Datalog limits |
| `notes/10-syntax-accuracy.md` | measured: 0 false positives on 6,291 files; 86 % exact repair of seeded paren errors |
| `notes/15-architecture-rules.md` | `steer rules`: Datalog over the require graph; what was tested, and why evaluation is ours |
| `notes/14-doc-lookup.md` | `steer doc`: catches wrong names; held-out top-1 4 → 13 of 20; what is measured and what is not |
| `notes/11-regular-grammar-languages.md` | conlangs and controlled English for steering; token measurements; G-series tools; chosen: acceptance criteria |
| `steer/` | the CLI (`main.rkt` entry; one module per concern) |
| `skills/` | Claude Code skills, embedded into the binary at compile time |
| `tests/` | rackunit unit tests and end-to-end CLI tests |
| `scripts/` | sample-data fetch/build, corpus gate, accuracy measurements |
| `.steer/` | this repo's own task store (backlog), committed |

## Known limits of the first slice

- `steer api` instantiates modules (their top-level code runs) in a plain subprocess; sandboxing is task T20.
- Clone detection works on surface syntax (not expanded code) and Racket only; non-Racket and near-miss clones are T18.
- Anchors in non-Racket files use a keyword/indentation heuristic and are labelled `~heuristic`.
- Task ids are sequential per store; two branches can both create `T7` (T21).
