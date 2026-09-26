# 15 · `steer rules`: architecture rules over the require graph (task T16, catalog F4)

Measured 2026-09-26, Racket 9.3 CS. Tags as in note 01.

## What it is [tested]

`steer rules check` reads `.steer/rules.dl`, extracts facts from the project's Racket sources, applies the
user's Datalog rules and reports each `violation(A, B, Message)` as a finding on file A, with the require path
that causes it and the line of the offending `require`. Example (a project where `ui/` must not reach `db/`):

```
error architecture-violation ui/page.rkt:2: the UI must not reach the database: ui/page.rkt → db/conn.rkt
  (via util/helper.rkt) → cut the chain, usually at its last link: remove the require of db/conn.rkt from util/helper.rkt
```

- **Facts** steer supplies: `module(M)`, `requires(A,B)` (project files), `uses(A,Lib)` (library requires),
  `layer(M,L)` from `%layer NAME GLOB` lines (Datalog comments, so the file stays valid Datalog), and `reach(A,B)`.
- **Extraction** is static (`read-syntax`, no expansion): `require`, `only-in`, `prefix-in`, `for-syntax`, `file`,
  `lib`, `submod`, also inside `module`/`module+`. Requires produced by macros are invisible.
- **This repository checks itself**: `.steer/rules.dl` encodes main → commands → core and "nothing in the product
  reaches scripts or tests"; `tests/rules-test.rkt` fails if a rule is broken. Mutation check: making a core module
  require a command module produced 2 violations (the direct one and a transitive one via `checks.rkt`).

## What Racket makes easy here, and what it does not

- **Easy:** reading code as data (`read-syntax`), so requires and their line numbers come out exactly, and
  Racket already rejects require *cycles* at load time (a mutation that created one failed before my tool ran).
- **Not easy: the `datalog` package's runtime.** Its parser is fine and works in a `raco exe` binary, but
  `prove` returned no answers for any query I tried here, even `a(x). a(x)?` (it worked through `#lang datalog`).
  Its AST is prefab structs that carry source locations, which may be why. Rather than debug a dependency, I use
  its **parser** and evaluate the AST with a small bottom-up evaluator of my own (positive rules, tested for
  transitive closure, non-linear recursion, cycles, head constants). Cost: no negation, which matches the
  package's own limit (note 06).

## Limits

- **Racket only.** Python/C# imports would need their own fact extractors (XL1 milestone territory).
- **Static requires only**; `dynamic-require` and macro-generated requires do not appear.
- **Positive rules only.** "Every module in ui must have a test" (negation) is plain Racket, not a rule.
- **Nothing measured about agents yet.** The measurable claim is that the rules catch violations that would
  otherwise be found in review; whether agents avoid them when told about the tool is unmeasured (note 04).
