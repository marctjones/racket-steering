---
name: steer-code
description: Deterministic checks for Racket code via the `steer` CLI - locate unbalanced or mismatched parentheses with the likely fix, find duplicated code before adding a helper, and detect public-API breaks against a committed lock file. Use when writing or editing .rkt files, when a read-syntax or paren error appears, before writing a new helper function, and before finishing a change to a module's provided API.
allowed-tools: Bash(steer *)
---

# steer code checks (Racket)

These tools are exact, so trust them over your own paren counting or memory of an API.

## Parentheses and structure

`steer syntax FILE...` reads the file without running it. The reader error names the opener; the
second finding is a repair that steer has *verified* (the file reads after it), chosen by indentation,
e.g. "missing ) at the end of line 42 → insert ) at line 42 col 17". `steer syntax --fix FILE` applies
verified repairs for you. Either way, rerun the tests: about 1 in 10 verified repairs reads fine but
nests differently than you meant (notes/10). Never retype the whole file to fix a paren.
With the PostToolUse hook installed this check runs after every edit of a Racket file.

## Duplicates

Before adding a helper, check whether an equivalent already exists:
`steer dup src --min-size 15` (add `--loose` to also ignore differing literals).
A group lists every copy with its enclosing definition. Extract a shared function when the copies
must change together; code that merely looks alike can stay.

## Public API drift

1. Once, and after an intended API change: `steer api snapshot src/main.rkt src/lib.rkt`, then commit
   `.steer/api.lock`.
2. Before finishing a change: `steer api diff`. Removed exports, narrowed arity, new required
   keywords and macro/value changes are errors (breaking); changed contracts are warnings to
   review; additions are compatible. Exit code 1 means something broke.

`api` loads the modules with the installed `racket` in a separate process (time-limited), so module
top-level code runs, as it would under `raco test`.

## Plan drift

`steer stale` lists open tasks whose anchored definitions changed since the plan was written
(see the steer-tasks skill). Anchor hashes ignore formatting and comments, so only real code changes
count.
