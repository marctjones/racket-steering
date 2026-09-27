---
name: steer-code
description: Deterministic code checks via the `steer` CLI, for Racket, Python and C# - locate unbalanced brackets/parens with the likely fix, resolve a symbol reference exactly (file.py#Class.method, file.cs#Class.Method), find dead code and public-API breaks over a shared call graph (all three languages), and (Racket only) find duplicated code. Use when writing or editing .rkt/.py/.cs files, when a syntax error appears, before writing a new helper function, and before finishing a change to a module's provided API.
allowed-tools: Bash(steer *)
---

# steer code checks (Racket, Python, C#)

These tools are exact for the languages listed below, so trust them over your own paren counting or
memory of a signature. For any other file type, `steer syntax` reports `skipped`, not `ok` — a clean
result never covers a language that is not in this table:

| language | syntax check | anchors (`file#Name`) |
|---|---|---|
| Racket (.rkt) | exact (the reader) | exact, by name; datum hash |
| Python (.py/.pyi) | exact (`compile()`, no import) | exact, by qualname (`Class.method`); needs python3 |
| C# (.cs) | exact bracket/string structure, no dotnet needed | exact, by qualname; overloads need `/arity` or `(types)` |
| anything else | skipped | heuristic (indentation), labelled `~heuristic`: a hint, not a guarantee |

## Parentheses and structure

`steer syntax FILE...` reads the file without running it (a subprocess for Python, none for Racket or
C#). The error names the opener; the second finding is a repair that steer has *verified* (the file
reads or compiles after it), e.g. "missing ) at the end of line 42 → insert ) at line 42 col 17".
`steer syntax --fix FILE` applies verified repairs for you. Either way, rerun the tests: a verified
repair sometimes nests differently than you meant (notes/10, notes/12). Never retype a whole file to
fix a bracket. With the PostToolUse hook installed this runs after every edit of a listed file type
and stays silent on file types with no gate.

## Does this symbol exist? What is it exactly?

For **Racket**, check a library function before you use it — a wrong name costs a whole edit-and-rerun
cycle. Quote names containing `?`, `*` or `!`, because zsh expands them as globs:

```
steer doc exists 'string-starts-with?'   # → not documented → did you mean string-prefix?
steer doc sig 'string-contains?'         # signature, argument contracts, (require ...), one-line description
steer doc search string contains         # names containing every word
steer doc exports racket/string          # what a module provides (also works on your own .rkt files)
```

A `not-in-racket` warning means the name exists only in another library (srfi, a teaching language):
require that library explicitly or use the racket/* name it suggests. Each lookup takes about a
second (it reads the installed docs), so use it for names you are unsure of, not for every call.

For **Python and C#**, a task's `--anchor path#Class.method` is resolved exactly against the real
code (an `ast` for Python, a member scanner for C#), not by indentation — decorators, multi-line
signatures and overloads all resolve to the right one. If a name is ambiguous (several overloads),
`steer add`/`edit` refuse with the exact candidates; disambiguate with `Name/arity` or `Name(int, string)`.
If the file exists but never defined the name, you get `anchor-unresolved` with a did-you-mean, not a
silent "pending" — a `path#name` for a file that genuinely does not exist yet is `anchor-pending`
instead, which is fine when the task itself creates it.

## Duplicates (Racket only)

Before adding a helper, check whether an equivalent already exists:
`steer dup src --min-size 15` (add `--loose` to also ignore differing literals).
A group lists every copy with its enclosing definition. Extract a shared function when the copies
must change together; code that merely looks alike can stay. Python/C# clone detection is not built
yet (catalog F5b); for those languages, search by hand before writing a new helper.

## The code graph: calls, entries, dead code (Racket, Python, C#)

`rules`/`api --entries` share one graph across all three languages, built from calls actually written
as calls (`f(...)`, `obj.f(...)`, `new T(...)`) — it never sees a plain value or attribute reference.
Every edge is tagged by how sure it is:

| language | same-file calls | cross-file calls | invisible to this graph |
|---|---|---|---|
| Racket | exact | declared (a resolved `require`: relative path, or a project-rooted collection spec) | a third-party struct-like macro's generated bindings (`define-object-type` and similar — not the built-in `struct`, which IS tracked) |
| Python | exact | declared (a resolved `import`), else name-match | a function/class used only as a VALUE: assigned to a variable then called through it, `functools.singledispatch` dispatch tables, a closure returned and invoked later, a bare attribute/property read |
| C# | exact | name-match only (no dotnet SDK, no namespace index: `using` almost never resolves to one file) | an attribute class used only via `[AttrName]` syntax; a bare field/property read |

**A `dead(S)` finding means "no call was found," never "this is unused"** — check the invisible column
above before deleting anything `steer rules dead` flags; on real projects measured this way, most dead
findings turned out to be exactly one of those patterns, not genuinely dead code (notes/16).

### Architecture rules

If the repo has `.steer/rules.dl`, `steer rules check` verifies layering rules over this graph (for
example "ui must not reach db") and prints the require/call *path* behind each violation, with the
line of the offending require. Fix the last link of the chain, not the first. `steer rules init` writes
an example; rules are positive Datalog plus `%layer NAME GLOB` lines (`steer help rules`). A rule can
also query `dead(S)`/`reachable(S)` directly, promoting a dead symbol in a given layer to a violation.

### Entry points and dead code

`steer rules entries` lists every entry point this graph found and which rule admitted it (an explicit
`;; steer: entry` / `# steer: entry` / `// steer: entry` marker, a language-specific heuristic like a
public method on a public class, or your own `entry("path#qualname").` fact/rule in `.steer/rules.dl`).
`steer rules dead` lists symbols nothing reaches from any entry point; `steer rules reach SYM` shows
which entries reach a symbol (with the shortest path each) and what it reaches in turn.

### Public API drift

`steer api snapshot --entries` records a static shape (real signature text) for every entry point
across every language, from the same graph; `steer api diff --entries` classifies changes the same
way for all three — removed/added parameters, an entry removed or demoted, and an arity mismatch at
an actual in-project call site. C# cannot yet tell an added *required* parameter from an added
*optional* one (no default-value tracking), so an added C# parameter is always classified breaking.

The older `steer api snapshot MODULE...` (no `--entries`) is a separate, per-module route: Racket
dynamically loads and instantiates the module with the installed `racket` (module top-level code
runs, as it would under `raco test`); Python reads `__all__`/leading-underscore via the same static
ast the graph uses. There is no non-`--entries` C# route.

## Test failures, readable

`steer done` parses pytest, `dotnet test` (VSTest) and `raco test` output: the failing test id,
`file:line`, and the first message line, capped at 3 with `--full`/`--limit` to lift it. A build or
import error (never reached its tests) is parsed too, from MSBuild diagnostics or a Python traceback.
An unrecognised runner falls back to the raw output tail, same as before this existed — don't assume
every check's failure is structured; check for `test-failed`/`diag-failed` findings vs a plain tail.

## Plan drift

`steer stale` lists open tasks whose anchored definitions changed since the plan was written
(see the steer-tasks skill). Anchor hashes ignore formatting and comments in every listed language,
so only a real code change counts. For a `~heuristic`-language anchor, treat `stale`'s silence as a
hint, not a guarantee: it can miss a body change hidden behind a multi-line signature.
