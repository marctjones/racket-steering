# 16 · The code graph, entry points and reachability: does it hold up on real projects?

Legend as in note 01/12: **[known]** documented behaviour, **[tested]** checked on this machine (2026-09-27, steer
built from this branch, Racket 9.3 CS, Python 3.13 + the stdlib `ast`), **[hyp]** hypothesis.

Method: the same four real projects note 12 already used, at the SAME pinned commits (`scripts/samples-sources.rktd`
for rebellion; note 12 §0's table for the other three), measured with one script (`scripts/graph-measure.rkt`) run
per language - no language gets its own measurement methodology, per the task's own requirement.

| project | language | commit | files | defs | refs |
|---|---|---|---|---|---|
| jackfirth/rebellion | Racket | `8f3fc467` | 186 | 1881 | 16183 |
| hukkin/tomli | Python | `5a77b12` | 14 | 80 | 471 |
| Tinche/aiofiles | Python | `5381a39` | 19 | 167 | 992 |
| ardalis/GuardClauses | C# | `f96b823` | 55 | 841 | 2187 |

## 1. Resolution ratio (exact + declared edges, of all edges) **[tested]**

| project | exact | declared | name-match | external | **resolution ratio** |
|---|---|---|---|---|---|
| rebellion (Racket) | 29.3% | 21.9% | 48.8% | 45.5%¹ | **51.2%** |
| tomli (Python) | 38.0% | 1.9% | 60.1% | 52.4%¹ | **39.9%** |
| aiofiles (Python) | 16.1% | 12.7% | 71.2% | 64.0%¹ | **28.8%** |
| GuardClauses (C#) | 13.8% | 0% | 86.2% | 24.5%¹ | **13.8%**³ |

¹ `external` overlaps with `name-match` (an edge can be both "resolved only by shared name" and "the target turned out
to be unresolvable" is not how the counts work - `external` counts edges where nothing in the project matched at
all, a subset that is *also* counted under whichever confidence tag it got, which for an unresolved edge is always
`name-match` with `to=#f`; so `external <= name-match` always, and the two are not mutually exclusive columns).

³ Superseded by §7 (the T63 attribute-refs follow-up, measured separately below), then again by §8 (the PARAMETER-
attrs follow-up): re-measuring after §7's fix gives exact 10.4%, declared 0%, name-match 89.6%, external 45.2%,
resolution ratio **10.4%** - LOWER, not higher, because the fix adds thousands of new `decorates` refs project-wide
(test-method `[Fact]`/`[Theory]` attributes are by far the largest source), and most of those correctly resolve to
nothing in-project (xUnit's own attribute classes, not GuardClauses'). More refs, correctly classified, is not the
same as better precision on the refs that matter - see §7 for why this is the right outcome, not a regression. §8
re-measures again after fixing parameter-level attrs AND deduping repeated attribute names per declaration (the
latter alone drops external from 45.2% to ~39.4%, before parameter attrs add anything): resolution ratio 11.5%.

**Racket has by far the best resolution ratio, and C# by far the worst - both make sense structurally, not just as
measurement noise:**

- Racket's `declared` share (21.9%) comes almost entirely from ONE fix made while measuring this (§4): resolving a
  `(require pkgname/sub/mod)` collection-style spec against the project's own root once its leading package-name
  segment is dropped. Before that fix, rebellion's declared share was **0%** and its resolution ratio was 29.7%, not
  51.2% - collection-style requires turned out to be rebellion's *dominant* intra-package require style, not a rare
  case.
- C#'s `declared` share is a real, structural **0%**, not a bug: the lang.rkt `resolve-import` contract is path-only
  (a language gets `(spec, importing-path, root, all-paths)`, never another file's own declared namespaces), and a
  `using Namespace.Sub;` almost never corresponds 1:1 to a `Namespace/Sub.cs` file layout in a real .NET project
  (GuardClauses' own `using` directives never matched this heuristic once). Every C# cross-file call is resolved
  purely by the graph's project-wide name-match fallback - which is *why* C#'s `name-match` share (86.2%) so far
  exceeds every other language's. This is the single clearest quantified case of "no language gets false precision
  it cannot back": C#'s numbers say plainly that its cross-file resolution is name-matching, not import-tracing.

## 2. Entries, by admitting heuristic **[tested]**

| project | marker | implicit | language prelude (public_api/test/console/controller) | total |
|---|---|---|---|---|
| rebellion | (see note) | - | 259 (public_api+root_module, test_name, console_script) | 259 |
| tomli | - | 5 (dunders) | 34 (public_api of `__init__.py`'s root module + test files) | 38²|
| aiofiles | - | 11 (dunders) | 101 | 112² |
| GuardClauses | - | 2 (`Main`/`ToString`-family) | 554 (public_api of public classes) | 556² |

² a symbol can be admitted by more than one source (e.g. an explicit marker AND public_api); `entries.rkt`'s own
`entry-admitted-by` records every contributing source, and the totals above are de-duplicated entry-symbol counts,
not a sum of the per-source columns.

None of the four fixtures happen to use an explicit `# steer: entry` / `;; steer: entry` marker (they are libraries
being measured as-is, not projects steer has been pointed at with real task annotations) - every entry above came
from an implicit-name or a language prelude rule. That is itself informative: **the entry-resolution engine's real
value on an unannotated real project is almost entirely in the language preludes**, not the marker mechanism (which
matters more once a project's own maintainers start using it deliberately, as note 12's own worked examples showed
for the anchor/task system generally).

### The `public_api`+`root_module` rule dominates every language's entry count, and needed a real fix in each one

This rule (`entry(S) :- public_api(S), in_module(S, M), root_module(M), lang(M, L)`) is - by a wide margin - the
largest single source of entries in every project measured. Getting it to fire *correctly* required a real,
language-specific fix in ALL THREE extractors, each found only by running this measurement for real:

- **C#** (found first, biggest effect): `exported?` was hardcoded `#t` for every def in cs-extract.rkt regardless of
  its `public`/`private`/`internal` modifier. Before the fix, GuardClauses showed **838 of 841 symbols dead** (99.6%)
  - not because the graph was wrong about reachability, but because NOTHING had ever been correctly admitted as an
  entry in the first place, public or not. Fixed by reading the real modifier keyword via a bounded backward token
  scan (stopping at a `{`/`}`/`;` boundary), with C#'s own real defaults when none is written (an interface member is
  public with no keyword; a class member defaults private; a top-level type defaults "internal-ish", i.e. visible
  project-wide, which is what matters for a same-project reachability check even though it is not part of the
  external public API). Also had to generalize `public_api`'s own definition: it originally required "no dot in the
  qualname" (true for a Python/Racket top-level def), which can NEVER be true for a C# method (every C# def nests
  inside a class) - so public_api never matched a single C# symbol until it was redefined as "exported, AND (no
  enclosing type, OR the enclosing type is itself exported)".
- **Racket** (found second, same shape of bug): `exported?` was ALSO hardcoded `#t` for every `define`/`struct` in
  rkt-extract.rkt, never actually checking `provide`. rebellion is a well-designed library that hides its internals
  and exposes a curated surface via `(provide (contract-out ...))` - before this fix (and before Racket even had
  its OWN public_api prelude rule, which also did not exist yet), rebellion showed **1017 of 1881 symbols dead**
  (54%). Fixed by a real (if not exhaustive) `provide` scanner: bare identifiers, `contract-out` (including
  `rename`), `rename-out`, `struct-out` (which provides a struct's generated constructor/predicate/accessors/
  mutators too, not just the struct name - and a struct name provided WITHOUT `struct-out` does NOT provide those,
  which the fix keeps distinct), `all-defined-out`, and `except-out`/`prefix-out` (approximated by descending into
  their sub-specs). After both fixes: rebellion's dead count dropped to 900 (still large - see §3).
- **Python** already tracked `__all__`/leading-underscore correctly (T54's own design got this right from the
  start), but its `public_api` helper had the SAME "no dot in qualname" bug as C# for a public method on a public
  CLASS (as opposed to a bare top-level function) - fixed by the same generalization.

This is the single most important finding of this measurement task: **an entry-resolution rule that looks correct
in isolation (and passed every unit test built for it) was silently near-useless on two of three real languages
until it was run on a real project and its actual entry/dead counts were read by a human.** Exactly the kind of gap
unit tests over small, hand-built fixtures cannot surface by construction - they are too small to accidentally
exercise "what if literally nothing is ever exported."

## 3. Dead-symbol counts, after the fixes above **[tested]**

| project | dead symbols | % of defs |
|---|---|---|
| rebellion | 900 / 1881 | 47.8% |
| tomli | 6 / 80 | 7.5% |
| aiofiles | 31 / 167 | 18.6% |
| GuardClauses | 212 / 841 | 25.2% |⁴

(Before the fixes in §2: rebellion 1017/1881 = 54.1%, GuardClauses 838/841 = 99.6%, tomli/aiofiles unaffected since
Python's public_api was already mostly working - their counts moved only slightly, from adding the implicit-name
dunders in §2's Python fix, covered in §4.)

⁴ Superseded by §7: after the attribute-refs follow-up, GuardClauses' dead count is 209/841 = 24.9%, a small real
drop (3 symbols), not the large one the §4 diagnosis below implied it should be - see §7 for why. Superseded again
by §8 (the parameter-attrs follow-up): 207/841 = 24.6% - see §8 for why this small a further drop is actually the
complete, correct fix, not an incomplete one.

## 4. Hand-checked dead-finding samples, classified true/false positive **[tested]**

20 per language where 20+ exist (GuardClauses, rebellion), all of them where fewer exist (tomli: 6, aiofiles: 31,
of which the first 20 by path-sort were inspected in full - the same handful of root causes recur throughout the
rest, checked separately, not one-by-one).

### Racket (rebellion): 20 sampled, all from `base/comparator.rkt`

**0 true positives, 20 false positives (0% precision on this sample).** Every one of them is a genuinely `provide`d
name (`comparator?`, `comparator-reverse`, `comparator-chain`, ... - all listed in `comparator.rkt`'s own
`(contract-out ...)`), reachable only via the ONE case this extractor deliberately does not special-case:
`define-object-type` (rebellion's own macro, roughly a fancier `struct`) generates a family of derived bindings
(the predicate, the real constructor under a different name, accessors) from ONE macro invocation the same way
`struct` does - but rkt-extract only special-cases the built-in `struct` form, not third-party struct-like macros.
Concretely: `(define-object-type comparator (function ...) #:constructor-name constructor:comparator)` gets read as
a single plain `define`-headed form naming `comparator`, which is never itself the identifier `contract-out`
actually provides (`comparator?`) - so this extracted `comparator` binding looks unprovided and unreachable, even
though the REAL, macro-generated `comparator?` predicate is both provided and (from outside this file) genuinely
part of rebellion's live public API. This is exactly note 12's own already-documented Racket gap ("macro-generated
names cannot be anchored"), now quantified: on this one real file, it is responsible for the entire sample's false
positives.

### Python (tomli, 6 of 6; aiofiles, first 20 of 31 inspected, same causes recur in the rest)

**0 true positives across both projects.** Every dead finding traces to one of three well-known dynamic-Python
idioms, none of which this extractor tracks (documented as `dynamic-calls?=#t` for Python since T64, now with real
examples and counts):

1. **A function assigned to a variable, then called through the variable** (tomli: `parse_escapes =
   parse_basic_str_escape[_multiline]`, then `parse_escapes(...)` later) - 4 of tomli's 6, cascading further to 2
   more (functions those functions call internally, which never "activate" because the outer function is itself
   unreachable by this analysis).
2. **`functools.singledispatch` + `@f.register(...)`** (aiofiles' `tempfile/__init__.py` and `threadpool/__init__.py`:
   every `@wrap.register(SomeType)` handler is named `_` and dispatched by the DECORATOR, never a literal call) -
   4 of the 20 aiofiles findings directly, cascading to 5 more (the wrapper classes those handlers construct).
3. **A closure created and returned as a value, called later via the returned reference** (tomli's
   `make_safe_parse_float`'s nested `safe_parse_float`; aiofiles' `wrap`'s nested `run`, and
   `delegate_to_executor`'s `cls_builder`) - the remaining findings in both projects.

A property accessor (`AsyncBase._loop`, `AsyncIndirectBase._file` getter+setter) is the SAME underlying gap in a
different shape: it is accessed as `self._loop`/`self._file`, a plain attribute reference, never a call - this
extractor only emits `ref`s for `Call` nodes, never for a bare `Name`/`Attribute` read. **This one limitation (calls
tracked, general value/attribute references not) is the dominant false-positive source for Python dead-code
detection on both real projects sampled**, and - see below - for C# too.

**After the fix in §2, but before this note's dunder-implicit-name expansion**, `__aiter__`/`__anext__`/`__repr__`/
`__await__`/`__aenter__`/`__aexit__` and similar object-protocol dunders were ALSO in aiofiles' dead list (the
original implicit-names table only covered `__init__`/`__new__`/`__enter__`/`__exit__`/`__call__`/`main` - the sync
context-manager protocol, not the async one, and not the general object protocol). Expanding the table (now:
`__del__`, `__aenter__`/`__aexit__`/`__aiter__`/`__anext__`/`__await__`, `__iter__`/`__next__`, `__repr__`/`__str__`/
`__format__`, the comparison/`__hash__`/`__bool__` family, `__len__`/`__getitem__`/`__setitem__`/`__delitem__`/
`__contains__`, `__getattr__`/`__setattr__`) dropped aiofiles' dead count from 44 to 31.

### C# (GuardClauses): 20 sampled (path-sorted), plus a full read of the remaining 192 by grep

**Superseded in part by §7, then §8**: the `decorates`-ref gap named just below WAS fixed, but only reduced this
project's dead count by 3 (212→209) - most of the 211 attribute-class false positives named here turned out to be
on PARAMETERS, not on the type/method declaration itself, a third tier §7's fix does not cover. Read on for the
original diagnosis, then §7 for what actually happened when it was acted on, then §8 for the parameter-level fix
and - the more important finding - why a full (not 20-sample) read of the dead list shows this project's own
attribute-class false positives were already down to essentially zero once §7+§8 are both in, not still ~211.

**0 true positives in the 20-sample; 1 true positive found in the full 212 (0.5% overall).** 211 of 212 are
attribute classes (`CallerArgumentExpressionAttribute`, and 17 in `ThirdParty/JetBrains.Annotations.cs`:
`NotNullAttribute`, `CanBeNullAttribute`, `PublicAPIAttribute`, ...) used ONLY via C#'s `[AttributeName]` syntax on
parameters and methods throughout the codebase - never as a named call anywhere. cs-extract already records a
type's own attributes as metadata (`def-decorators`, feeding the `decorated(S, D)` fact), but never emits a graph
EDGE from a decorated symbol back to the attribute class itself (Python's extractor does emit a `decorates` ref for
this; C#'s does not) - a real, identified gap, not fixed in this pass given the time remaining in this milestone.
The ONE genuine finding: `TestObj._internalValue`, a private field read as a bare identifier
(`_internalValue.CompareTo(...)`, `other?._internalValue`) inside its own class's methods - the SAME "calls tracked,
plain references not" gap Python's property accessors hit, confirmed here as a cross-language limitation, not a
Python-specific one.

## 5. What this means, plainly

- **The entry-resolution engine's biggest win (public_api+root_module) needed a real per-language visibility fix in
  two of three languages before it worked at all**, found only by running it for real (§2). This is now fixed for
  Racket and C#, verified by the before/after dead counts.
- **Dead-code precision on this sample is poor for both Python and C#** (0% true positives sampled for Python; 0.5%
  for C#), and **also poor for Racket** on the one file sampled (0%), all for the SAME reason at bottom: this graph
  only ever emits a `ref` for something written as a call (`f(...)`, `obj.f(...)`, `new T(...)`), never for a plain
  value/attribute reference, a decorator/attribute application (Python and, since §7/§8, C# on the type/member AND
  parameter tiers are the exceptions here - all three now emit `decorates` refs), or a
  name captured by a language runtime hook this project's `implicit-names` table does not
  yet list. **`steer rules dead`'s findings should be read as "no call to this was found," not "this is unused" -
  a human (or an agent) still has to look**, exactly as `notes/12`/`notes/16`'s own framing for this whole system
  says: an over-approximation, never a claim of precision no static analysis without a real compiler can back.
- **The resolution-ratio numbers are language-BLIND about *how* they got so different**: C#'s low ratio is not a
  bug so much as an honest reflection that "no dotnet SDK, no namespace index" means cross-file C# resolution really
  is closer to guessing by name than tracing imports; Racket's number is what happens when your import statements DO
  correspond to real files (once collection-style ones are resolved). This asymmetry is a real, useful signal for
  anyone deciding how much to trust a `declared`-confidence edge on one language versus another.
- **Performance was noisy while measuring, and worth a plain note rather than a polished-away one.** After the
  collection-style-require fix, `link-facts` on rebellion (1881 defs, 16183 refs) measured anywhere from ~1s to
  ~12s across repeated runs in this same session, and this repo's own `rules-projection-test.rkt` (a much smaller
  project) grew from ~900ms to several seconds over the course of this same session without any further code
  change in between some of those runs. The most likely explanation, following note 12's own precedent ("13.4s was
  CPU contention with a parallel `dotnet` build; alone the same check takes 0.7s"), is contention with other work
  running on the same machine during this session, not a real algorithmic regression - `link-facts` with a no-op
  resolver on the same project measured a stable ~1s, and the collection-style resolver adds real but bounded extra
  work (2-3 path-resolution attempts per import statement, not per reference). This is flagged as an open question
  for whoever picks this up next, not resolved here: worth a dedicated, isolated benchmark run rather than trusting
  any single wall-clock number from a shared, busy machine.

## 6. What changed from the design, for the record

- T58's `gate` struct kept the name `gate` (not renamed to `lang`) to stay byte-compatible with existing callers;
  recorded on T58 directly.
- T60/T63's Racket/C# `shape` fields started as placeholders (`"(f ...)"` / `"method Dog.Speak"`) and were only
  given real formals/paramtypes in T67, once shape-lock needed something real to diff - not a design change, but
  worth noting these two tasks' own `#:check`s never exercised the shape TEXT, only its presence.
- C# cannot tell an added REQUIRED parameter from an added OPTIONAL one in a shape diff (T67): `cs-extract`'s
  `paramtypes` are type-only (T50's own `param-type-text` strips defaults, by design, for anchor purposes) - a real,
  permanent limitation of the current extractor, not something T67 rounds away.
- T54 (Python API lock) was implemented as part of this milestone even though it is tracked outside T58-T70,
  because T67 has a real, tracked dependency on it.

## 7. Follow-up (post-milestone, 2026-09-27): C# attribute-usage `decorates` refs **[tested]**

§4's diagnosis (211 of 212 GuardClauses false positives are attribute classes with no `decorates` edge) was acted
on: `cs-extract.rkt` now emits a `decorates` ref for every `[AttrName]` usage on both a type declaration (already
tracked as metadata via `cs-type-attrs`; now also an edge) and, new, a MEMBER declaration (`cs-member` gained an
`attrs` field, threaded from `scan-members`'s existing `skip-prefix/attrs` - previously a separate, name-dropping
`skip-prefix` was used for members specifically, discarding the very names `cs-type-attrs` was already collecting
for types). Ref scope is the decorated symbol's ENCLOSING type, matching `steer/python.rkt`'s own decorator-ref
convention (`self._decorator_refs` runs before the decorated def's own name is pushed onto the scope stack) - not
the decorated symbol's own qualname.

**A real resolution problem, found before it shipped, not by measurement:** `[AttrName]` almost always omits the
class's own "Attribute" suffix (`[Obsolete]` for `class ObsoleteAttribute`), confirmed against GuardClauses' actual
source before writing any code (`git clone` at the pinned commit, grepped directly) - every real usage checked
omits it. Since which form is real can only be known once the WHOLE project's defs are visible, not from the one
file being extracted, the extractor emits BOTH the name as written and, when it lacks the suffix, the suffixed
form, as two separate refs. Whichever one is not a real project symbol resolves as an ordinary external ref (same
as any other unmatched call) - this stays entirely inside `cs-extract.rkt`, no linker change, since the suffix
convention is a C# lexical fact, not a general graph-resolution rule. Verified mechanically: a new fixture case
(`tests/fixtures/csproj/Shapes.cs`'s `LoudAttribute`, used as `[Loud]`) asserts exactly one RESOLVED `decorates`
edge out of the two emitted, and that the attribute class becomes reachable once its decorated member's enclosing
type is (`tests/cs-graph-test.rkt`).

**The measured real-world effect was much smaller than the diagnosis implied, and that gap is itself the finding.**
Re-measuring GuardClauses at the same pinned commit (`f96b823`) after the fix:

| | dead symbols | resolution ratio (exact/declared/name-match/external) |
|---|---|---|
| before (§1/§3) | 212 / 841 (25.2%) | 13.8% / 0% / 86.2% / 24.5% |
| after | 209 / 841 (24.9%) | 10.4% / 0% / 89.6% / 45.2% |

Only 3 fewer dead symbols, not the ~211 §4's diagnosis implied - confirmed real (`NotNullAttribute`, resolved via
its own member-level usage `[NotNull] public string FormatParameterName { get; }`, no longer appears in the dead
list at all). The resolution ratio DROPPED (13.8%→10.4%), not from a regression, but because the fix surfaced
thousands of new refs project-wide - the dominant new source, by a wide margin, is xUnit's `[Fact]`/`[Theory]`
attributes on test methods throughout `test/GuardClauses.UnitTests/`, which correctly resolve as external (xUnit's
own attribute classes, not GuardClauses'). More refs, correctly classified as unresolvable, dilutes a ratio computed
over all edges; it is not evidence the fix is wrong, and `external` (45.2%, up from 24.5%) grew for the same reason.

**Why the fix barely moved GuardClauses specifically: most real attribute usage in this project is on PARAMETERS,
not on the type or member declaration itself** - a THIRD tier, distinct from both §4's original diagnosis and this
fix's scope, found only by re-measuring against the real fixed graph rather than assuming the diagnosis was
complete. Confirmed directly against the source (same clone, same commit): `CallerArgumentExpressionAttribute` is
used exclusively as `[CallerArgumentExpression("input")]` on a trailing method PARAMETER
(`GuardAgainstZeroExtensions.cs` and every sibling `GuardAgainst*Extensions.cs` file, dozens of call sites), and
most of the 17 JetBrains annotations (`[NotNull]`, `[ItemNotNull]`, `[CanBeNull]`, ...) are likewise applied to
parameters inside a constructor's or method's own parameter list, not to the method as a whole. `scan-members`'s
existing `skip-prefix/attrs` only ever runs at a member's OWN start, before its return type; it has no visibility
into attributes written on individual parameters inside `(...)`, and `parse-params`/`param-type-text` (T50) work
on raw token TEXT, not indices, so they cannot reuse `attr-names-in` (which needs the token vector and indices) as
written. This is a real, previously-undiagnosed scope cut, not fixed in this pass, and is a plausible reason C#'s
false-positive rate would stay high on other real .NET projects that lean on parameter-level attributes the same
way (`[FromBody]`, `[Required]`, ASP.NET model-binding attributes are the same shape) - flagged via `spawn_task` as
a follow-up rather than silently left as a gap in this note alone. → §8 acts on this follow-up; read there for the
fix and, more importantly, for how much of "most of the 17 JetBrains annotations... are likewise applied to
parameters" turned out to be true attribute-class USAGE at all once checked against the real source, versus
attribute classes GuardClauses vendors but never itself applies anywhere.

**What this adds to §5's framing, plainly:** a fix that is mechanically correct and verified end-to-end on a
purpose-built fixture (proven: the mechanism works, the suffix-guessing is necessary and sufficient for the case it
targets) can still fail to move a real project's numbers much, because the original diagnosis from a 20-symbol
hand-sample generalized past what that sample actually contained. **A false-positive rate is not one root cause
just because the sample looked uniform** - GuardClauses' 211 "attribute class" false positives were actually two
different causes (type/member-level attributes, now fixed; parameter-level attributes, not yet), and the sample
size that found the number (211) was too small, and too concentrated in one codebase's own conventions, to tell
them apart. The same caution note 12 and §4 already applied to false-positive RATES applies just as much to
false-positive CAUSES: verify the fix against the real project it was diagnosed from before calling it fixed, not
just against a fixture built to exercise the mechanism.

## 8. Follow-up (post-milestone, 2026-09-27): C# PARAMETER-level `decorates` refs, and what a full dead-list read
shows **[tested]**

§7's own closing note flagged a third, undiagnosed tier: most real attribute usage in ardalis/GuardClauses is on
PARAMETERS (`[CallerArgumentExpression("input")]`, most JetBrains annotations), which `scan-members`'s
`skip-prefix/attrs` never sees (it only runs at a member's own start, before the parameter list) and which
`parse-params`/`param-type-text` (T50) could not collect either, since they work on raw per-parameter TEXT, not
token indices, so they could not reuse `attr-names-in` (which needs `tv`+indices) as written.

**The fix**: `parse-params` (steer/csharp.rkt) now tracks each parameter's start index as it splits on top-level
commas (the SAME depth-tracking loop it already had, extended, not duplicated), and treats a `[...]` group at zero
depth before any other token of the current parameter (`cur` is still empty) as an attribute list - reusing
`attr-names-in`, the same collector scan-types' own type-attribute scan uses - rather than pushing those tokens
into the parameter's own text. `cs-member` gained a `param-attrs` field, kept separate from `attrs` (the member's
OWN attributes): `def-decorators` reads `attrs` alone, and a parameter attribute is not a decorator of the member
itself. `cs-extract.rkt` emits a new `member-param-attr-refs` list, same convention as the existing member/type-
level ones: scope is the decorated member's ENCLOSING type (there is no parameter-level symbol in this graph to
scope to individually), at the member's own start line.

**A real bug fixed as a side effect, pinned by a test, not just noticed**: before this, the attribute's own tokens
leaked into `param-type-text`'s output - `[CallerArgumentExpression("input")] string? name = null` produced the
shape text `[CallerArgumentExpression("input")]string?`, not `string?`. Skipping the `[...]` group (not just
collecting its names) fixes this; the fixture asserts `def-shape` on a parameter-attributed method directly.

**A second fix, incidental to the first but real**: `attr-decorates-refs` now dedupes `names` before emitting
refs. Without this, the SAME attribute name applied to several parameters of one method (`[NotNull]` on three
parameters, say) would emit its own pair of duplicate ref structs per occurrence - and since `attr-decorates-refs`
is shared by all three tiers (type/member/parameter), this ALSO fixes a pre-existing inflation at the member
level: GuardClauses' own test suite stacks `[Theory]`/`[InlineData(...)]` repeatedly on single test methods (620
`InlineData` usages project-wide), and each repeat was previously counted as its own pair of (correctly-external)
refs.

**Verified with a fixture, the same style as §7's**: `tests/fixtures/csproj/Shapes.cs` gained `CheckedAttribute`
(used bare, on a parameter of `Dog.Fetch`, with `Fetch` carrying NO member-level attribute of its own - the only
way a resolved `Dog -> CheckedAttribute` edge can exist is via the new per-parameter path) and `SilentAttribute`
(on a parameter of the already-unreached `UnusedHelper`, mirroring `QuietAttribute`'s negative case).
`tests/cs-graph-test.rkt` asserts: the resolved edge exists and is exact via the suffix fallback (same as
`LoudAttribute`); `CheckedAttribute` is reachable (Dog is reachable) and `SilentAttribute` is not (Program is
not); `Fetch`'s own `def-decorators` is `'()` (proof the parameter attribute did not leak into member-level
metadata); and `def-shape` is `"Fetch(string)"`, not the bracketed form (pinning the shape-text bug above). `raco
test tests/` (1597 tests) and `make test-bin` both pass.

**Re-measuring GuardClauses at the same pinned commit (`f96b823`), fresh clone, `.steer/` cache cleared before
EACH run** (a stale content-hash-keyed cache from an earlier run on the SAME clone silently served pre-fix facts
to a later run during this work - content hashing has no way to know the EXTRACTOR itself changed, only that the
`.cs` files didn't; a real trap hit while measuring, not a hypothetical one, worth remembering for anyone
re-measuring after touching `cs-extract.rkt`/`csharp.rkt`):

| | dead symbols | resolution ratio (exact/declared/name-match/external) | refs | edges |
|---|---|---|---|---|
| before (§7's state, re-measured fresh: dedup fix present, param-attrs still disabled) | 209 / 841 (24.9%) | 11.5% / 0% / 88.5% / 39.4% | 3777 | 7792 |
| after (this fix) | 207 / 841 (24.6%) | 11.5% / 0% / 88.5% / 39.5% | 3981 | 8035 |

The "before" row here is NOT bit-for-bit §7's own recorded 10.4%/0%/89.6%/45.2%: the dedup fix above lowers
`external` even WITHOUT parameter attrs (fewer duplicate `[InlineData(...)]`-stack refs), so re-measuring §7's own
state fresh, with dedup but param-attrs still off, already shows 11.5%/39.4%. Most of §7→§8's `external` drop
(45.2%→39.5%) is the dedup, not the new parameter refs, which barely move the ratio at all (39.4%→39.5%) because
they resolve almost entirely to names dedup was already collapsing duplicates of.

**Only 2 dead-list entries move (209→207), and diffing the two dead lists names them exactly**:
`CallerArgumentExpressionAttribute` (the class) and its own constructor. `NotNullAttribute` and
`ValidatedNotNullAttribute` - the other two attribute names GuardClauses' own code actually applies to a
parameter - were ALREADY reachable before this fix, for reasons that have NOTHING to do with parameter attrs:
`ValidatedNotNullAttribute` is directly entry-admitted (a `public` top-level type, the `public_api` rule from §2),
and `NotNullAttribute` is reached through a pre-existing, somewhat coincidental chain (`ValidatedNotNullAttribute
: Attribute`'s base-class reference name-matches an unrelated property literally called `Attribute` inside
`AspRequiredAttributeAttribute`; that property's own module then cascades reachability, via `reach.rkt`'s "a
reachable symbol's own module is reachable too" rule, to every OTHER type-level `decorates` edge already in that
same vendored file - including `BaseTypeRequiredAttribute -> NotNullAttribute`, a genuine member-level attribute
use that already existed since §7). This is a real, pre-existing over-approximation risk in the graph (a name
collision on "Attribute" as a base-class reference, cascading through file-level reachability) worth its own
look some day, but it predates this fix and is not something this follow-up introduces or needs to fix to be
correct and complete for its own stated scope.

**A full read of the dead list (207 items, not a 20-sample) is the real finding, and it reframes §4/§7 rather
than just extending them**: 201 of the 207 are `Attribute`-suffixed, all from the ONE vendored file
`ThirdParty/JetBrains.Annotations.cs` (95 attribute classes there, all correctly `internal` - checked directly
against the source, ruling out an `exported-type?` bug); 5 more are enum types that are only ever used as
constructor-parameter TYPES of those same unused attribute classes; the last is `TestObj._internalValue`, §4's
own already-documented "plain reference, not a call" gap, unrelated to attributes entirely. Cross-checking real
usage across the WHOLE project (grepping every `.cs` file except each class's own defining file, `src` AND
`test`) turns up exactly THREE non-BCL attribute names GuardClauses' own code ever applies, ALL parameter-only:
`CallerArgumentExpression` (39 sites), `NotNull` (23), `ValidatedNotNull` (22) - and all three resolve as
reachable after this fix (two of them, as above, already did before it for unrelated reasons). **Zero remaining
false positives among attribute-class dead findings in this project.** The other 200 (195 attribute classes + 5
enums) are not a resolution gap at all: they are true dead code, unused portions of a large vendored third-party
file GuardClauses ships but barely uses (ASP.NET MVC/Razor/XAML tooling-hint attributes like `AspMvcController`,
`RazorDirective`, `XamlItemBinding`, entirely irrelevant to a guard-clause library).

**What this means for §4's own framing, plainly**: §4 read a 20-item, path-sorted sample and reported "0 true
positives in the sample, 1 in the full 212 (0.5%)" - correct as far as it went, but a 20-sample cannot distinguish
"the extractor has a resolution gap" from "this symbol is genuinely, permanently unused," and §7's diagnosis (and
this note's own §7 text) generalized the sample's uniform "attribute class" SHAPE into an assumption that fixing
attribute-usage refs would resolve most of the COUNT. It would have, IF most of those 211 had been real usage the
extractor was failing to connect - they were not. The measured effect (209→207) looks small only if you expected
the diagnosed count (211) to equal the count of real false positives; once every one of the 207 is actually read
and cross-checked against real source usage, this fix is complete for GuardClauses: every attribute this
project's own code applies to a parameter now resolves. The caution note 12/§4/§7 already apply to false-positive
RATES and CAUSES applies with equal force to false-positive COUNTS: a sample telling you "which shape the false
positives have" does not tell you "how many of that shape are real."

**Remaining, real sub-tiers this fix does not cover** (none of them explain ANY of GuardClauses' own 207 -
confirmed above - so these are scope notes for other .NET projects that might lean on them, not residual causes
here): attributes on a positional record's or primary constructor's parameters (`scan-types`/`scan-members` both
`skip-group` straight past that parameter list, T63/T50's own original scope cut); indexer parameters
(`finish-property` never parses the `[...]` argument list at all, arity or attrs); attributes on a lambda's or
local function's parameters (body token ranges are only scanned for calls, never re-parsed for declarations);
attributes on a generic type parameter (`<[Foo] T>`); and `[return: X]` (targets the return value, not the
declaration - `attr-names-in` would collect `return` itself as a bogus attribute "name" and miss `X`, a real,
separate bug in the EXISTING member/type-level scan from §7, not something this fix touches).
