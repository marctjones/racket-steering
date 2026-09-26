# 12 · Cross-language review: does `steer` help on Python, Racket and C# projects?

Legend as in note 01: **[known]** documented behaviour, **[tested]** checked on this machine (2026-09-26, steer 0.1.0
from `dist/`, Racket 9.3 CS, Python 3.14.5 + pytest 9.1, .NET SDK 10.0.401), **[hyp]** hypothesis.

Question (the user's): do the tools so far help steer an AI coding agent on Python, Racket or C# projects, and what
should be built next? Method: `steer init` in one real project per language, a realistic 3-5 task plan with the
language's own test runner as `#:check`, anchors on real definitions, `done`/`verify` with real failures, and the
hooks on non-Racket edits. Plan: `scratchpad/cross-language-plan.rktd` (§7).

## 0. Setup

| language | project (pinned commit) | check command | isolation |
|---|---|---|---|
| Python | hukkin/tomli `5a77b12` (+ Tinche/aiofiles `5381a39`, read-only, for async/decorator shapes) | `PYTHONPATH=src python3 -m pytest` (17 passed, 744 subtests) | system python3 + pytest were already installed; nothing added |
| C# | ardalis/GuardClauses `f96b823` (xunit, 959 tests) | `DOTNET_ROLL_FORWARD=Major dotnet test test/GuardClauses.UnitTests` (test project targets net8.0; only the net10 runtime is installed) | `dotnet restore` and the Roslyn 4.14 probe package wrote to the shared `~/.nuget/packages` cache (revert: `dotnet nuget locals all --clear`); build output stayed in the clone |
| Racket | jackfirth/rebellion from `samples/corpus`, copied | `raco test base/result.rkt` | `raco pkg install --link` under an isolated `PLTADDONDIR` in scratch; its dependency `guard` came from the package catalog |

Everything ran against a copy of `dist/` and scratch `.steer` stores; this repo's own store was only read.

## 1. Verdict

| tool | Python | C# | Racket |
|---|---|---|---|
| tracker: import, graph, next/claim, checkpoint, resume, since | helps as is [tested] | helps as is [tested] | helps [tested] |
| `done` / `verify` with the native runner | runs; the pytest tail is usable only because `-q` ends with a summary [tested] | runs, 11-105 s; the tail shows the *last* 2 of 10 failures and clips `file:line` [tested] | runs; the rackunit block fits the tail [tested] |
| anchors `file#name` | partial: found, but wrong extent and first match wins [tested] | weak: generics, partial classes, properties, namespaces all "not yet created" [tested] | exact; macro-generated names invisible [tested] |
| `stale` | misses body and decorator edits, flags comment edits [tested] | misses body edits [tested] | right under comments and reformatting [tested] |
| `syntax` | false errors on valid files [tested] | false errors on valid files [tested] | 186/186 rebellion files ok, 0.9 s [tested] |
| post-edit hook | silent (exit 0) on `.py` [tested] | silent on `.cs` [tested] | works [known] |
| `dup` | "0 files" (T18) [tested] | "0 files" (T18) | 10 groups in 186 files, 3 s, real duplicates [tested] |
| `api` | load-failed (reads it as Racket) [tested] | n/a | correct classification on a real change [tested] |

- **Racket**: everything applies. Gaps found: macro-generated names cannot be anchored; `api` needs the package's
  dependencies installed because it instantiates modules.
- **Python**: the tracker and executable checks help now; anchors are unreliable in exactly the shapes black-formatted
  code has; no syntax gate, no hook, no API lock, no clones.
- **C#**: the tracker helps; anchors mostly fail; checks cost 10-100 s, so they belong in `done`, not in hooks; no
  syntax gate, no API lock.

## 2. Evidence

### Python (tomli, aiofiles)

```
$ steer import plan-py.rktd      → imported 4 tasks … critical path (3): T2 → T3 → T5
$ steer verify T3                → pass PYTHONPATH=src python3 -m pytest -q tests/test_misc.py -k parse_float (0.7s)
$ steer done T2                  (error message format changed on purpose)
not done: 1 of 1 check failed on T2
error check-failed T2: `PYTHONPATH=src python3 -m pytest -q tests/test_error.py` exited 1 after 13.4s
    …
    tests/test_error.py:98: AssertionError
    FAILED tests/test_error.py::TestError::test_line_and_col - AssertionError: 'I...
    FAILED tests/test_error.py::TestError::test_tomldecodeerror - AssertionError:...
    2 failed, 5 passed in 0.65s
```
Usable, but only because pytest's summary is last and short; the messages are pytest-truncated. (13.4 s was CPU
contention with a parallel `dotnet` build; alone the same check takes 0.7 s.)

Anchors as `steer show` prints them; real extents from the stdlib `ast` in brackets:
```
_parser.py#parse_array          L519-521  [519-542]  multi-line signature: block ends at the "):" line
_parser.py#parse_inline_table   L545-547  [545-573]  same
_parser.py#__init__             L100-106  [100-147]  first of three __init__ wins, and ends at "):"
_parser.py#NestedDict.__init__  not yet created      qualified names unsupported → treated as "the task creates it"
_parser.py#safe_parse_float     L793-797  ok         nested def found
_parser.py#Flags, #loads, #set  ok                   class, function, method (single-line signatures)
threadpool/__init__.py#wrap     L99-101   [98-101]   @singledispatch line excluded
threadpool/__init__.py#_        L105-106  first of four singledispatch registrations
threadpool/__init__.py#_open    L66-78    [66-95]    async def found; body excluded
```
`steer stale` after single edits on a clean checkout: decorator `@singledispatch → @functools.singledispatch`: 0 stale
(miss); a comment line inside `_open`: 1 stale (false alarm); body of the second `_`: 0 stale (miss); body of `open`
below its signature: 0 stale (miss); a string literal inside `wrap`: 1 stale (right).

```
$ steer syntax src/tomli/_parser.py    → error read-error :1:1: read-syntax: bad syntax `# ` … exit 1   (valid file)
$ echo '{"tool_input":{"file_path":"broken.py"}}' | steer hook post-edit   → exit 0, no output
$ python3 -c 'import ast,sys; ast.parse(open(sys.argv[1]).read(), sys.argv[1])' broken.py
  → SyntaxError: invalid syntax, line 1, offset 12 (75 ms)
$ steer dup src                        → 0 clone groups in 0 files
$ steer api snapshot src/tomli/_parser.py   → load-failed: read-syntax: bad syntax `# `
```

### C# (GuardClauses)

```
$ steer verify T2      → pass … dotnet test … --filter FullyQualifiedName~GuardAgainstNullOrEmpty (41.5s warm, alone;
                         104.6s on the first run, under CPU contention with a parallel build)
$ dotnet build src/GuardClauses --no-restore -f net8.0     → 10.8 s unchanged; 5.2 s with a syntax error
$ steer done T2        (empty-string guard broken: 10 of 32 tests fail)
    tail = last 2 failures + "Failed!  - Failed: 10, Passed: 22 …"; the `in /…/GuardAgainstNullOrEmpty.cs:line N`
    part is clipped by the 200-char line limit, so no file:line reaches the model
```
Anchors:
```
GuardAgainstNullExtensions.cs#Null                   not yet created   generic `Null<T>(`: the C rule needs `name(`
GuardAgainstNullExtensions.cs#NullOrEmpty            L92-96  [92-106]  signature only: Allman `{` after a multi-line signature ends the block
GuardAgainstNullExtensions.cs#GuardClauseExtensions  not yet created   `partial` is not a known modifier
Guard.cs#Against                                     not yet created   property (no parenthesis)
Guard.cs#Ardalis.GuardClauses                        not yet created   file-scoped namespace
GuardAgainstExpressionExtensions.cs#Expression       not yet created   generic, [Obsolete]-decorated twin in another file
Guard.cs#Guard, #IGuardClause                        L14-22, L6-8 ok
```
`steer syntax Guard.cs` → false `unclosed` error, exit 1. Hook silent on `.cs`.

A Roslyn worker as a .NET 10 single-file app (`#:package Microsoft.CodeAnalysis.CSharp@4.14.0`,
`dotnet run decls.cs -- FILE`) lists every member with kind, generic arity, parameter types, line span and a
whitespace-normalised hash, and reports `22:13 CS1026 ) expected` for the broken file: 9.9 s first run (compile +
fetch), 1.0-1.9 s warm. `dotnet build` prints the same diagnostic as
`Guard.cs(22,13): error CS1026: ) expected [proj::TargetFramework=net8.0]`. [tested]

### Racket (rebellion)

```
$ steer syntax $(find . -name '*.rkt')     → 186 ok, 0 findings, 0.9 s
$ steer dup .                              → 10 clone groups in 186 files, 3.1 s (repeated test tables, begin-for-syntax blocks)
$ steer api snapshot base/result.rkt base/option.rkt   → 12 + 17 exports with contracts, 6 s
$ steer api diff   (option-get no longer provided; option-map given an optional argument)
error removed-export base/option.rkt: option-get: export removed (breaking) → restore it, or re-snapshot if the break is intended
info arity-widened base/option.rkt: option-map: arity 2 → 2|3 (compatible)
warning contract-changed base/option.rkt: option-map: contract (-> option? (-> any/c any/c) option?) → (->* …) (review compatibility)
$ steer done T1    (call/result broken)    → location: base/result.rkt:105:4  actual: (failure 3)  expected: (success 3)
```
Anchors: `base/result.rkt#success` (define-tuple-type) L35 ok; `#success-value` (macro-generated accessor) "not yet
created"; a comment plus reformatting inside `result-case` → 0 stale. A contract narrowed past what the definition
satisfies comes back as `load-failed: option-map: broke its own contract`: right, but it reads like a tool failure.

## 3. Failure modes, ranked by how often an agent would hit them

1. **The heuristic block ends at the signature.** Any multi-line parameter list (black-formatted Python; C# with an
   attribute per parameter) ends the block at `):` or before the Allman `{`; body edits never make the plan stale.
   [tested] Cause: `block-end` takes a same-indent `)`/`}` as the closer and accepts `{` only as the *first* body line.
2. **First match wins.** Overloads (C#), `__init__` in several classes, singledispatch `_` registrations: edits to any
   but the first are invisible, and qualified names are not accepted. [tested]
3. **Unresolvable looks like not-yet-created.** A typo, a qualified name or a generic method silently becomes
   `pending`; `add`/`import` print an `info`, `resume` says "not yet created". [tested]
4. **Nothing runs on `.py`/`.cs` edits**, and `steer syntax` on them gives confident, wrong findings. [tested]
5. **Check output is a tail, not a parse.** dotnet test loses the first failures and the `file:line`; pytest depends
   on `-q`; only rackunit fits. [tested]
6. Decorators and attributes sit outside the hash (Python, C#); comments sit inside it (text hash). [tested]
7. C# checks take 10-100 s: nothing per-edit may shell out to `dotnet`. [tested]
8. Racket: macro-generated names cannot be anchored; `api` needs the package's deps installed. [tested]

## 4. What would help most, per language

The project's rule stands: exact, fast, one finding the model can act on. For non-Racket code the exact facts come
cheapest from the language's own parser; Racket stays the host that normalises, hashes, stores and routes.

- **Python: Racket front end, facts from the stdlib `ast`.** No install: python3 is present whenever the project is
  Python. One embedded worker script (the pattern `api.rkt` already uses) returns, in 60-80 ms for the 799-line
  `_parser.py`, every def/class with qualified name, decorator-inclusive span, `end_lineno`, and a hash over
  `ast.dump(node, include_attributes=False)`: the exact analogue of the Racket datum hash (comments and formatting
  invisible, docstrings visible). [tested] The same worker yields `SyntaxError(msg, lineno, offset, end_lineno,
  end_offset)` with 3.10+ messages ("'(' was never closed") for a syntax gate [tested], and `ast.unparse(args)`
  signatures plus `__all__` for a static API lock that never imports the module [tested], consistent with
  `srcread.rkt`'s read-without-running stance (`inspect.signature` would execute it).
- **C#: Racket for the fast path, Roslyn for the exact path.** A brace- and string-aware C-family scanner in Racket
  (verbatim, interpolated and raw strings; `//`, `/* */`; `#if`) fixes anchors (generics, `partial`, properties,
  attributes, Allman bodies, overloads named `Name/arity` or by parameter types) and gives a sub-50 ms structural
  gate for the hook. [hyp] Exact declarations and diagnostics come from the single-file Roslyn worker (1 s warm,
  needs .NET 10 SDK) or from `dotnet build` output parsed into findings, on demand only (`done`, `--deep`). [tested]
- **Racket: keep.** Anchoring macro-generated names only if measured demand appears (expansion-based, T19 territory).
- **All three:** route `hook post-edit` and `steer syntax` by extension with an explicit `skipped` finding for
  unknown languages; parse runner output (pytest, dotnet test, rackunit, MSBuild `file(line,col): error CS…`) into
  at most 3 located findings per check with the raw tail behind `--full`; on `add`/`import`, warn with did-you-mean
  when the file exists but the name does not.

## 5. Candidates evaluated

| candidate | verdict | why |
|---|---|---|
| tree-sitter anchors | not now [hyp] | one native grammar per language inside a `raco exe` binary, and no maintained Racket binding found; `ast` and Roslyn are exact and already installed where they matter. Revisit for a fourth language. |
| `python -m py_compile` / `ast.parse` gate | yes [tested] | 75 ms, located, good messages; a machine-applicable edit only for the known shapes (never closed, missing `:`). |
| Roslyn / `dotnet build` diagnostics | yes, on demand [tested] | same text either way; build 5-11 s, worker 1 s warm / 10 s cold. Never in the hook. |
| Python API lock via `inspect.signature` | no; static `ast` instead [tested] | it executes the module; `ast.unparse` gives the same signature text without running anything. |
| C# public API via PublicAPI.Shipped/Unshipped.txt | thin [hyp] | with the analyzer on, RS0016/RS0017 come out of `dotnet build`, which the diagnostics parser covers; otherwise a Roslyn declaration dump is the baseline. Lowest priority. |
| language-agnostic clones | T18 | already planned (token winnowing). |
| test-runner output parsers | yes [tested] | §2; with hook routing the best value per effort. |
| hook routing by extension | yes [tested] | trivial; unblocks everything else. |

## 6. Not worth it

- Re-implementing Python or C# parsers in Racket beyond the brace scanner; the native parsers are exact and present.
- Tree-sitter, for now (above).
- A per-edit `dotnet build` hook (5-40 s per edit).
- `inspect`-based Python API locks, an MCP server or a warm server process before the parsers exist.
- Anchoring Racket macro-generated names (rare in plans; needs expansion, T19).

## 7. Plan

Plan file `cross-language-plan.rktd` (delivered with this review, not committed; `steer import` it), tag
`cross-language`, dry-run validated against a copy of this store: 14 tasks, every anchor resolves.

- **XL1 "Cross-language steering v1"** (check: an end-to-end test over Python and C# fixtures; C# cases skip when
  `dotnet` is absent, as in CI): hook routing by extension → Python syntax gate via `ast` → C# structural scanner →
  test-output parsers (pytest, dotnet test, rackunit) → MSBuild/Python diagnostics parser → exact Python anchors →
  C# scanner anchors → anchor did-you-mean → language-aware skill and hook docs.
- **XL2 "Cross-language API locks and clones"**: static Python API lock, single-file Roslyn worker, C# public-API
  baseline, plus the existing T18.
