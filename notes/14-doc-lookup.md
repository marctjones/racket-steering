# 14 · `steer doc`: documentation and signature lookup (task T12, catalog B1)

Measured 2026-09-26, Racket 9.3 CS. Tags as in note 01. Reproduce: `racket scripts/doc-hallucination.rkt`.

## What it is

`steer doc exists|sig|search|exports` answers "does this name exist, and what is its real signature?" from
Racket's own documentation instead of the model's memory. [tested]

- **Facts** come from the documentation index (`setup/xref`: `xref-index`, `xref-binding->definition-tag`,
  `xref-tag->path+anchor`) and the installed docs' HTML (the documented signature with argument contracts, and the
  description that follows a group of definitions), plus `value-contract` / `procedure-arity` read inside the
  library's namespace (contracts are invisible from other namespaces; see note 06).
- **Where it runs:** a worker in the *installed* `racket`, like `steer api`. A standalone `raco exe` binary cannot see
  the docs. Cost: about 1 s per lookup (0.26 s when a MODULE is given; the index load is 0.6 s).
- **Also works on your own modules:** `steer doc exports path/to/file.rkt` lists names, arity and contracts, marked
  `(undocumented)`.
- Same output rules as the rest of steer: one located finding with a fix, `--json`, exit 1 when the name is unknown.

## Two facts that changed the design

1. **"Documented somewhere" is the wrong test.** `fold`, `select`, `1+`, `string-index` and `string-contains` exist in
   SRFI or teaching-language libraries, so a plain existence check says "yes" for names a `#lang racket` program cannot
   use. `exists` now warns (`not-in-racket`) when no `racket/*` library has the name, and suggests racket names.
2. **Lexical distance cannot find the right name.** `string-starts-with?` is far from `string-prefix?`. Suggestions
   now combine (a) a small table of other-language phrases (`starts-with`→`prefix`, `get`→`ref`, `1+`→`add1`), (b)
   shared name tokens with the leading type word (`list-`, `hash-`, `string-`) split off, so `list-sort` matches
   `sort`, and (c) edit distance, drawn from `racket/*` names only.

## Measured: is the right name suggested? [tested, with caveats]

Pairs are our guesses at what a model writes, not observed model output. Rank = position of the right name in
the (at most five) suggestions.

| set | pairs | flagged | top-1 | top-3 | top-5 | status |
|---|---|---|---|---|---|---|
| dev: T3 mutation list | 28 | 28 | 23 | 27 | 27 | the phrase table was designed looking at these |
| fresh | 20 | 20 | 14 | 15 | 15 | its failures motivated the verb-token change |
| **fresh2** | 20 | 20 | **13** | **14** | **14** | written before the change; never used for design |

On fresh2, the version before the ranking change gave top-1 4, top-3 9, top-5 11. Plain edit distance on the whole
index (the first version, 5 of 28 right on the dev set) is worse still.

The ranking change also caused a regression that the dev set exposed and I fixed the same day (`hash-set` was
classed a "bare type name" because both words are type words; top-5 dropped from 28 to 25 before the fix).

## What is left, and what this does not show

- **Remaining misses are vocabulary**, not ranking: `any`→`ormap`, `all`→`andmap`, `find`→`findf`, `items`→`hash->list`,
  `delete`→`remove`, `length`→`hash-count`. Adding them now, from these sets, would only overfit. They should be
  added from real model failures (E1 logger, T7); the table is data in `steer/doc.rkt`.
- **Whether a model writes fewer wrong names when it can call this tool is unmeasured.** The measured part is: given a
  wrong name, does the tool catch it and point at the right one. Models under-use optional tools (note 05 question 2):
  the hook route (task T44: run a check after every edit) is the way to make this automatic, e.g. by extracting
  identifiers from an edit and running the `exists` test without being asked.
- **Speed:** about 1 s per lookup is fine per unfamiliar name, too slow to run on every identifier in a file. A cached
  name index on disk would make bulk checks cheap; not built.
- **zsh globs `?`**: `steer doc sig string-contains?` fails with "no matches found" before steer runs. The skill says
  to quote such names.
