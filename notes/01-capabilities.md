# 01 · Racket capabilities that matter for steering

Legend: **[known]** = documented Racket behaviour I am confident of; **[hyp]** = hypothesis about usefulness for LLM
steering, needs testing. Verify API names against the current docs before building; treat names below as pointers.

## 1. Code is data (homoiconicity)

- `read-syntax` returns a syntax object with source locations for every form. [known]
- A generated program can be parsed, walked, rewritten and pretty-printed without regexes. [known]
- **Steering use:** exact line/column error reports; structural edits ("wrap this form", "rename this binding") applied by
  a tool instead of by the model re-typing text. Model emits an edit description, tool applies it, so no unbalanced
  parentheses are possible. [hyp]
- **Limit:** the reader cannot help with text that is not s-expressions (e.g. the model wrapped code in prose or
  markdown fences). Needs a pre-pass that extracts code.

## 2. The expander knows what names mean

- Fully expanding a module (`expand`, `expand-syntax`, `syntax/parse`, `identifier-binding`) resolves every identifier
  to its binding, module and phase. [known]
- Unbound identifiers, wrong-arity calls to known functions (partly via the compiler, and via Typed Racket fully),
  and `require`s of nonexistent modules surface at expansion time, before any run. [known]
- **Steering use:** an "does every name exist?" check is the single most likely hallucination catcher. Return the
  unbound name plus nearest matches from the exports of the required modules. [hyp]
- **Limit:** expansion runs arbitrary macro code from the modules it loads. It must run inside a sandbox (see 03).

## 3. Documentation as queryable data

- Racket's documentation is built from Scribble; `scribble` and the `racket/help` / documentation index tools expose
  binding → doc mappings and signatures (contracts appear in the docs). [known, exact API to confirm]
- `syntax/parse` classes and `provide` contracts carry machine-readable signatures. [known]
- **Steering use:** "signature + one-line description + example for identifier X" and "what in library Y does Z?" as a
  retrieval tool, giving the model real signatures instead of remembered ones. [hyp: probably the highest value per
  line of code]
- **Limit:** third-party packages vary in doc quality; examples may not run.

## 4. Contracts with blame

- `racket/contract` attributes a violation to a specific party (which module broke the promise) and shows the
  value and the expected predicate. [known]
- **Steering use:** wrap generated functions in contracts derived from the task spec; the failure message tells the
  model *which side was wrong* and gives the offending value. Far more informative than a stack trace. [hyp]
- **Limit:** cost at runtime (measured in this project: contracts are 8x slower than plain calls under the iOS
  interpreter). Fine for checking, not for shipping.

## 5. Typed Racket (gradual typing)

- Type errors carry expected/actual types and locations. Typed/untyped boundaries auto-generate contracts. [known]
- **Steering use:** ask the model to write typed code and use the type checker as the oracle; or infer types for
  untyped snippets to catch mismatches. [hyp]
- **Limit:** LLMs write typed Racket less well than untyped (less training data), and occurrence typing errors can be
  cryptic. Measure before assuming it helps.

## 6. Macros and `#lang`: restricted languages for agents

- A `#lang` or a macro-based DSL can make illegal programs unrepresentable or un-expandable, with custom error
  messages written by us. [known]
- **Steering use:** give the agent a *small* language for a task class (data pipeline steps, tool-call plans,
  workflows). The model writes in that language; the checker rejects out-of-policy programs at expansion time.
  Smaller grammar = fewer ways to be wrong, and error messages are ours to design for LLM consumption. [hyp]
- **Limit:** the model has never seen the language, so the prompt must carry a spec plus examples; measure whether
  that costs more than it saves.

## 7. Sandboxing and resource limits

- `racket/sandbox` (evaluators with memory and time limits, restricted filesystem/network via security guards and
  custodians). [known]
- **Steering use:** trial-run generated code safely and return only structured results (value, stdout, error,
  time, memory). [known feasible]
- **Limit:** not a hard security boundary against a hostile program; use OS-level isolation (container, VM,
  sandbox-exec) for untrusted code. Sandbox is for accidents, not adversaries.

## 8. Property-based testing and generators

- `rackcheck` and `redex` provide random generation of structured data and of well-formed terms. [known that they
  exist; check current status of each]
- **Steering use:** derive generators from contracts/types, generate inputs, shrink failures to a minimal
  counterexample and return it. [hyp]
- **Redex** specifically can generate random programs from a grammar: use for differential testing of an
  LLM-written interpreter/transformer against a reference. [hyp]

## 9. Solver-aided programming (Rosette)

- Rosette lifts a subset of Racket to symbolic values and asks an SMT solver: verify, find counterexamples,
  synthesize holes, angelic execution. [known]
- **Steering use:** (a) check a generated function against a spec on all inputs up to a bound; (b) let the model
  write the *sketch* and the solver fill constants/branches; (c) equivalence of rewrite vs original. [hyp]
- **Limit:** works on a restricted, loop-bounded subset; scales badly on big code; spec writing is the real cost.

## 10. Introspection of the running system

- Namespaces, `dynamic-require`, `module->exports`, `module->namespace`, `current-command-line-arguments`,
  `PLT_*` env, profiler, `errortrace`, `racket/trace`. [known]
- **Steering use:** enumerate a module's exports/contracts, coverage (`errortrace` coverage), profile output as
  structured data. [hyp]

## 11. Tooling already in `raco`

`raco make` (compile errors), `raco test`, `raco check-requires`, `raco fmt`/`raco expand`, `raco pkg`, `raco docs`,
`raco cover`. [known, confirm each is installed]. These are free steering signals; the work is *packaging their
output as structured, minimal, stable JSON* for the agent.

## What is NOT a Racket advantage (be honest)

- Racket has far less LLM training data than Python/JS, so the model starts weaker; tools must compensate.
- Nothing here needs Racket specifically for *checking other languages*; the strongest case is steering the writing
  of Racket (or of a Racket-hosted DSL) itself.
- Neural or statistical steering (rerankers, learned verifiers) is not a Racket strength. Keep Racket for the exact
  parts.
