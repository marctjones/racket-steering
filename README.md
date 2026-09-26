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
| continuity | `resume` (budgeted packet) · `since N` (event cursor) · `graph` (cycles, layers, critical path) | F1 |
| plan drift | `stale` / `refresh`: symbol anchors (`file#name`) hashed over the datum, so reformatting is not drift | F2 |
| Racket code | `syntax` (paren errors + likely fix line) · `dup` (clones modulo renaming) · `api snapshot/diff/show` | A1 F5 F3 |
| harness | `skills install` · `hook session-start` · `hook post-edit` · `hook config` | F7 |

What makes it agent-friendly: `done` runs the task's checks and refuses on failure; `checkpoint` requires
`--did` and `--next`, so a cleared context can continue from `steer resume`; plans are imported
all-or-nothing with located errors and did-you-mean fixes; every mutation is locked and logged.

### Build and install

Needs Racket 9 (tested on 9.3 CS).

```bash
make          # build/steer (raco exe)
make test     # 110 tests: unit + end-to-end CLI
make test-bin # the end-to-end suite against the compiled binary
make install  # dist/ (self-contained) + symlink in ~/.local/bin (PREFIX=... to change)
```

The distributed binary runs without Racket installed, except `steer api`, which loads user modules
with the installed `racket` in a time-limited worker process.

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
| `steer/` | the CLI (`main.rkt` entry; one module per concern) |
| `skills/` | Claude Code skills, embedded into the binary at compile time |
| `tests/` | rackunit unit tests and end-to-end CLI tests |
| `.steer/` | this repo's own task store (backlog), committed |

## Known limits of the first slice

- `steer api` instantiates modules (their top-level code runs) in a plain subprocess; sandboxing is task T20.
- Clone detection works on surface syntax (not expanded code) and Racket only; non-Racket and near-miss clones are T18.
- Anchors in non-Racket files use a keyword/indentation heuristic and are labelled `~heuristic`.
- Task ids are sequential per store; two branches can both create `T7` (T21).
