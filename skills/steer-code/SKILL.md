---
name: steer-code
description: Deterministic code checks via the `steer` CLI, for Racket, Python and C# - locate unbalanced brackets/parens with the likely fix, resolve a symbol reference exactly (file.py#Class.method, file.cs#Class.Method), and (Racket only) find duplicated code and detect public-API breaks against a committed lock file. Use when writing or editing .rkt/.py/.cs files, when a syntax error appears, before writing a new helper function, and before finishing a change to a module's provided API.
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

## Architecture rules (Racket only)

If the repo has `.steer/rules.dl`, `steer rules check` verifies the layering rules over the Racket require
graph (for example "ui must not reach db") and prints the require *path* behind each violation, with the
line of the offending `require`. Fix the last link of the chain, not the first. `steer rules init` writes
an example; rules are positive Datalog plus `%layer NAME GLOB` lines (`steer help rules`).

## Public API drift (Racket only)

1. Once, and after an intended API change: `steer api snapshot src/main.rkt src/lib.rkt`, then commit
   `.steer/api.lock`.
2. Before finishing a change: `steer api diff`. Removed exports, narrowed arity, new required
   keywords and macro/value changes are errors (breaking); changed contracts are warnings to
   review; additions are compatible. Exit code 1 means something broke.

`api` loads the modules with the installed `racket` in a separate process (time-limited), so module
top-level code runs, as it would under `raco test`. There is no Python or C# equivalent yet
(catalog F3/XL2): don't assume `steer api` covers those languages.

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
