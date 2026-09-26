# racket-steering

Notes and (eventually) tools for **steering LLM agents with deterministic Racket programs**: validators, checkers,
retrieval, capability languages and verifiers that an agentic system calls while it writes code or processes data.

Status: **notes only** (started 2026-09-26). Nothing here is implemented or measured yet. Every claim about what
helps an LLM is a hypothesis until the evaluation in `notes/04-evaluation.md` has been run.

## The idea in one paragraph

An LLM proposes; a deterministic tool disposes. The tool does not need to be smart. It needs to be *exact*, *fast*,
and to return feedback in a form the model can act on (a location, a rule that was broken, a counterexample, the
correct signature). Racket is an unusually good host for such tools because the language is its own AST (code is
data), its expander can answer "what does this identifier mean here?", its contract system attributes blame, its
macro system lets us define small checked languages for agents to write in, and Rosette adds a solver.

## Contents

| file | what it holds |
|---|---|
| `notes/01-capabilities.md` | Racket capabilities that are useful for steering tools, why, and their limits |
| `notes/02-tool-catalog.md` | concrete tool ideas, each with inputs, outputs, feedback format, build effort and value guess |
| `notes/03-architecture.md` | how the tools reach an agent (CLI, MCP server, hooks), the shared feedback protocol, sandboxing |
| `notes/04-evaluation.md` | how to test whether any of this helps, benchmarks, metrics, controls |
| `notes/05-prior-art-and-questions.md` | related work to read, open questions, risks, non-goals |

## Suggested first slice

(1) structural + binding validator, (2) blame-returning check loop, (3) documentation/signature lookup, packaged as
one CLI, then run the evaluation in note 04 with and without it. Decide the rest from those numbers.
