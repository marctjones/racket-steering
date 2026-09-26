# 05 · Prior art to read, open questions, risks

## Related work to read (from memory; verify each exists and its current status before relying on it)

- **Constrained decoding:** grammar-constrained generation (e.g. llama.cpp GBNF, Outlines, guidance/lm-format-enforcer
  style libraries). Question: can a Racket reader/expander-derived grammar be exported as a GBNF for balanced
  s-expressions plus a bound-identifier whitelist?
- **Execution feedback for code models:** self-repair / self-debugging papers (repair from compiler and test output),
  AlphaCode/CodeT-style test-based selection, RLHF/RL from execution feedback.
- **Verifier-guided search:** program synthesis with neural guidance (DreamCoder, library learning), Rosette and
  Sketch-style synthesis, Herbie (Racket) as an example of a deterministic rewriter with an accuracy oracle.
- **Language-server / tool-use for agents:** LSP-driven agents, tree-sitter based edit tools, the MCP ecosystem.
  Compare: what does an LSP-style "diagnostics + go to definition + hover" already give? Racket has `racket-langserver`
  (check state); our tools add expander-level binding facts, contracts/blame, doc retrieval.
- **Racket-specific:** `syntax/parse` (structured macro errors), `errortrace`, `raco cover`, Redex generators,
  `rackcheck`, Typed Racket, Rosette, `racket/sandbox`, Scribble doc index, `racket-mode`/langserver diagnostics.
- **LLM Racket performance:** look for existing benchmarks of LLMs on Racket (MultiPL-E includes Racket
  translations of HumanEval/MBPP; check the reported pass rates for Racket vs Python). This gives a baseline for T1.

## Open questions

1. How much of LLM failure on Racket is *knowledge* (wrong APIs; fixable by lookup) vs *reasoning* (wrong algorithm;
   not fixable by these tools)? Measure with the taxonomy before building big tools.
2. Do models actually use optional tools? Evidence from agent work suggests hooks/auto-injection beat "available
   tools". Test C3 vs C4.
3. Can structural edit tools (path-based edits) beat text diffs for parenthesis correctness without confusing the
   model? Needs a format the model handles reliably.
4. Is a restricted DSL a net win once the spec must be taught in-prompt? Possibly only for repeated task classes
   where the spec/examples are cached.
5. Does Typed Racket as oracle help or hurt small models?
6. What is the right granularity for feedback (one error vs all)? Try both.
7. Can cheap deterministic checks be used as *rerankers* over multiple sampled candidates (generate N, keep those that
   expand and pass tests)? Probably a strong, simple baseline: measure early.
8. Can the same tools serve non-Racket targets (e.g. checking generated Python via a bridge)? Out of scope for now.

## Risks

- **False confidence:** a checker that misses errors trains the agent to trust bad code. Report what was *not*
  checked.
- **Misleading feedback:** wrong suggestions can send the model down a wrong path; measure false-positive harm.
- **Expansion runs code:** a validator that expands untrusted modules is an execution vector. Sandbox always.
- **Overfitting to our benchmark:** keep hold-out tasks, add fresh ones periodically.
- **Model drift:** results are tied to model versions; record and re-run.
- **Scope creep into building a language server or an agent framework:** keep tools small and composable.

## Non-goals (for now)

- Training or fine-tuning models.
- A general agent framework.
- Replacing existing linters/LSP; we complement them with expander/contract/doc-level facts.
- Steering non-Racket languages.

## Next steps

1. Decide repo name/visibility (currently local only; no remote).
2. Build E1 (logger) and A1 (structural validator) as the smallest useful slice.
3. Assemble T1-T3 with a hold-out; run baselines C0/C1 on 2-3 models to get the failure taxonomy.
4. Then A2, B1, A3; re-run; decide.
