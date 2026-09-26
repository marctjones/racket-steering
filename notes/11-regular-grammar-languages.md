# 11 · Regular-grammar languages, controlled English and spec tools

Legend as in note 01: **[known]** documented behaviour or published result, **[tested]** checked on this machine
(Racket 9.3 CS, 2026-09-26), **[hyp]** usefulness hypothesis to be measured by note 04's method.

Status (2026-09-26): design note. The only measurements are the token proxies in §3; every "helps a model"
claim below is [hyp] until the plan in §6 runs (results go to `notes/13-criteria-results.md`).

The question: can steering tools use *regular* artificial languages (Esperanto, Lojban, Toki Pona) or controlled
natural languages (Attempto Controlled English, EARS), and does parsing a human language into a regularised
grammar help (a) steer a model and (b) cut token costs?

## 1. What "regular" buys in each language

"Regular" means three different things, and only one of them helps a checker:

| sense | Esperanto | Lojban | Toki Pona | ACE / EARS |
|---|---|---|---|---|
| **morphology** without exceptions | yes: 16 rules, every noun `-o`, plural `-j`, object `-n`, tense by ending [known] | yes: self-segregating morphology, word boundaries are unambiguous from the phoneme string [known] | trivial: ~120 (pu) to ~137 (ku) words, no inflection [known] | English morphology, unchanged |
| **syntax** with one parse per sentence | no: free word order, prepositional attachment and scope are as ambiguous as in any natural language; `je` is a wildcard preposition [known] | yes: grammar formally specified (YACC baseline, PEG proposal; `camxes` parsers), a text has one parse tree [known] | no formal grammar; particles `li`/`e`/`pi` mark structure; community parser exists [known] | yes: a fixed subset of English syntax with *interpretation rules* (PP attaches to the verb, relative clause to the preceding noun, coordination precedence `and > or > ,and > ,or`, quantifier scope by textual order, anaphora by recency) [known] |
| **semantics** that a tool can evaluate | no | partly: predicate place structures are fixed, but tanru (compound modifiers) are explicitly vague [known] | no: vagueness is the design goal [known] | ACE: yes, deterministic translation to DRS/first-order logic/OWL by APE (SWI-Prolog). EARS: no formal semantics, only five clause shapes (ubiquitous, WHEN, WHILE, IF-THEN, WHERE) [known] |
| LLM exposure | mid-resource: eo.wikipedia ≈ 389 k articles, 271 k Tatoeba en-eo pairs, in FLORES-200 (`epo_Latn`) [known] | tiny: 12 k Tatoeba pairs | tiny: 14 k Tatoeba pairs | English |
| tokens vs English (§3) | 1.44-1.71× on English-centric tokenizers, 1.15× on XLM-R [tested proxy] | 1.50-1.72× | 1.72-1.94×, 1.57× on XLM-R | ≈ 0.9× for EARS/ACE-style prose |

None of these is "regular" in the Chomsky sense (type 3); that sense matters only for constrained decoding, where
engines take regexes and context-free grammars. The property a steering tool needs is **determinism of the
parse plus a semantics the tool can act on**, and that is exactly what a language *we* design has for free
(s-expressions read by `read-syntax` have one parse and source locations [known]). Esperanto's regularity is in
the wrong layer (morphology), Lojban's is real but comes with no corpus, Toki Pona trades precision for size.
The controlled-English family (ACE, EARS, Gherkin) is the only one whose regularity is in the layer that
produces *checkable* statements, and it stays inside the language the model is strongest in.

## 2. Use cases for parsing into a regularised grammar

### (a) Steering

| use | what the deterministic part does | what the model does | evidence |
|---|---|---|---|
| **checkable goals and acceptance criteria** | parse each criterion (EARS shape), refuse vague ones with a located fix, require a covering check before `done` | writes criteria in the template; writes the checks | EARS in Rolls-Royce practice and in Kiro's spec-driven development, adopted for LLM agents [known]; effect on agents unmeasured [hyp] |
| **unambiguous specs** | ACE-style interpretation rules make attachment and scope fixed, so a spec means one thing to the tool and to a later reader | writes in the subset | ACE → DRS is deterministic [known]; whether models write valid ACE without many retries [hyp] |
| **controlled prompts** | the tool *renders* a compact structured form into prose for humans (zero model tokens) and back | reads/writes the compact form | rendering is trivial; net effect [hyp] |
| **validating model outputs** | plans, criteria, checkpoints and notes must parse; errors come back with line:col and a template | retries against the error | `steer import` already does this for plans (all-or-nothing, located, did-you-mean) [tested] |
| **grammar-constrained decoding** | export the grammar as GBNF/JSON schema so a local model *cannot* emit an invalid form | nothing | llama.cpp GBNF, Outlines, XGrammar exist [known]; the Claude API offers JSON-schema structured outputs and `strict` tools, not arbitrary grammars [known from the API reference] |
| **facts and rules over parsed specs** | Datalog: which task covers requirement R, which requirement no test covers; Rosette: are two WHEN/SHALL clauses contradictory within a bound | writes the requirements | Datalog coverage query works, negation does not, so "uncovered" is plain Racket [tested]; Rosette part [hyp], L effort |

### (b) Token costs, concretely

- **Reading:** the model reads a spec once per turn, so the per-token price of its form is paid on every turn
  that carries it. A cached prefix (a taught grammar, a skill) costs a fraction of a fresh input token; vendor
  pricing pages give the rate [known; rate varies by model].
- **Writing:** output tokens cost several times input tokens (5× on the current Claude price table [known]).
  What the model must *write* dominates. A compact form the tool can check and render is cheaper twice: fewer
  output tokens, and fewer retries because the checker rejects early.
- **Translation is overhead:** a human-language intermediate that no tool checks adds a model call in, a model
  call out, and nothing in between that a tool can act on.
- **Reasoning quality:** MGSM shows lower accuracy in low-resource languages (part of the gap was later shown
  to be evaluation artefacts, MGSM-Rev2) [known]; Llama-2 internals are English-biased (Wendler et al. 2024)
  [known]; prompting SWE-bench Lite in Chinese did not save tokens and lowered success (Mythbuster, 2026)
  [known]. The metric that matters is note 04's **cost per solved task**, not tokens per prompt.
- **BPE premium:** tokenizers trained on English-heavy data split other languages into more pieces (Petrov et
  al. 2023, up to 15× for some scripts) [known]. §3 shows Esperanto pays 1.44-1.71× with English-centric
  tokenizers and 1.15× with a multilingual SentencePiece one, so the premium is a tokenizer property, not a
  language property; but the tokenizer is not ours to choose.

Where a regularised *English* subset or a compact structured form can win: criteria and plans that the model
writes and the tool checks; prompts whose fixed part is cached; JSON replaced by s-expressions or a keyword
form when the consumer is our own tool (§3: the `(task ...)` form is 41 % cheaper than the same task as JSON).

## 3. Measurements (proxies; Claude's tokenizer is not available offline)

Method [tested]: OPUS Tatoeba v2023-04-12 (CC BY 2.0 FR) aligned pairs en-eo, en-jbo, en-toki; 3000 pairs with
distinct English sentences per language, seed 0; tokenizers: tiktoken `cl100k_base` (GPT-4) and `o200k_base`
(GPT-4o), and HF `tokenizer.json` for Qwen2.5-7B, DeepSeek-V3, Mistral-Nemo, BLOOM, XLM-R, and
`Xenova/claude-tokenizer` (a community re-upload of a Claude-2-era tokenizer, **not** the current one; column
`claude-old*`). Anthropic's API reference says tiktoken undercounts Claude tokens by ~15-20 % on typical text
and more on code and non-English; the exact count comes from `POST /v1/messages/count_tokens`, not run here.
Tatoeba sentences are short and simple; ratios on technical text are higher (below).

Ratio = target tokens / English tokens over the same 3000 pairs:

| pair | words tgt/en | chars tgt/en | cl100k | o200k | Qwen2.5 | DeepSeek-V3 | Mistral-Nemo | BLOOM | XLM-R | claude-old* |
|---|---|---|---|---|---|---|---|---|---|---|
| en-eo | 0.93 | 1.02 | 1.67 | 1.45 | 1.61 | 1.60 | 1.44 | 1.54 | 1.15 | 1.71 |
| en-jbo | 1.18 | 1.03 | 1.55 | 1.52 | 1.55 | 1.57 | 1.51 | 1.54 | 1.50 | 1.72 |
| en-toki | 1.47 | 1.23 | 1.92 | 1.73 | 1.92 | 1.87 | 1.82 | 1.72 | 1.57 | 1.94 |

Esperanto uses *fewer words* than English (0.93) and the same characters, yet 1.5-1.7× the tokens: 2.2
tokens/word against 1.25 for English under `cl100k`. Toki Pona needs 1.47× the words to say the same thing, so it
loses before tokenization.

One task spec in parallel forms, author-written (n = 1, a proxy for form, not for translation quality; the texts
are below so the numbers can be rechecked):

| form | chars | cl100k | o200k | Qwen2.5 | DeepSeek-V3 | Mistral-Nemo | BLOOM | XLM-R | claude-old* |
|---|---|---|---|---|---|---|---|---|---|
| English prose | 426 | 88 | 88 | 88 | 89 | 88 | 88 | 105 | 88 |
| EARS | 377 | 81 | 79 | 81 | 86 | 86 | 81 | 98 | 85 |
| ACE-style | 362 | 77 | 77 | 77 | 77 | 77 | 77 | 91 | 81 |
| Gherkin | 458 | 103 | 103 | 103 | 103 | 103 | 93 | 100 | 93 |
| s-expression | 219 | 65 | 65 | 65 | 68 | 68 | 57 | 81 | 67 |
| JSON | 396 | 117 | 117 | 117 | 118 | 118 | 118 | 126 | 120 |
| Esperanto prose | 457 | 168 | 144 | 165 | 165 | 148 | 153 | 136 | 176 |

The README's plan example: `(task ...)` form 57 tokens, YAML 54, English prose 62, JSON 97 (`cl100k`).

```
English prose: Add CSV export to the report module. The export should write one row per record, with a
  header row taken from the record keys. It must refuse to overwrite an existing output file unless the
  caller passes a force flag, and in that case the existing file must stay unchanged. If the input is
  empty, the export writes a file containing only the header row. The tests must cover the header row,
  the force flag and the empty input.
EARS: The report module SHALL export records as CSV, one row per record. / The export SHALL write a header
  row taken from the record keys. / WHEN the output file exists and force is not set, the export SHALL
  refuse and leave the file unchanged. / WHEN the input is empty, the export SHALL write only the header
  row. / The tests SHALL cover the header row, the force flag and the empty input.
ACE-style: The report module exports every record as a row of a CSV file. / The header row contains the
  keys of the records. / If the output file exists and the force flag is not set then the export fails and
  the output file is not changed. / If the input is empty then the export writes only the header row. /
  The tests cover the header row and the force flag and the empty input.
s-expression: (spec csv-export (module report) (row-per record) (header (keys record))
  (when (and (exists? out) (not force)) (refuse) (unchanged out))
  (when (empty? input) (writes header-only)) (tests header force empty))
Gherkin: Feature: CSV export, three Scenarios with Given/When/Then/And lines (14 lines).
JSON: the s-expression's content as an indented object with "rules": [{"when": ..., "then": [...]}].
Esperanto: the prose, translated by the author (unchecked by a native speaker).
```

Reading: controlled English (EARS, ACE-style) saves 8-12 % over prose, which is noise-level; its value is
checkability. A compact structured form saves ~26 %, and beats JSON by a third. Gherkin costs *more* than prose
(keywords and indentation). Esperanto costs 1.6-2.0× on technical text with the English-centric tokenizers
(1.3× on XLM-R), more than on Tatoeba, because technical vocabulary fragments further.

## 4. What is not an advantage (be honest)

- **No Esperanto, Lojban or Toki Pona front-end.** Measured: more tokens, not fewer. Known: less training
  data, so worse reasoning, plus a translation step no tool can check. Lojban's unambiguous parse is the one real
  property, and s-expressions have it with vastly more model exposure (Lojban: 12 k Tatoeba pairs).
- **Controlled English does not save tokens.** 8-12 % on one sample is within noise. It buys a parse, and the
  parse buys checks. Sell it as that.
- **S-expressions beat JSON, not YAML.** The `(task ...)` form is a third cheaper than JSON but not cheaper than
  YAML (57 vs 54 tokens). Its advantage is one reader with source locations, not raw token count.
- **A parser checks form, not truth.** "The export SHALL refuse" parses whether or not the code refuses. The
  tool has teeth only when every clause must be covered by an executable check and `done` refuses otherwise
  (note 06's rule: the model declares, the tool enforces).
- **The model has to learn the subset.** ACE's construction rules are a manual; EARS is five templates. Every
  taught rule costs prefix tokens on every turn and may cost retries; the false-reject rate (valid goals the
  grammar refuses) is the number to measure first, and it is deterministic to measure.
- **Constrained decoding is not available on the Claude API** beyond JSON schemas and strict tool inputs
  [known from the API reference]. A grammar tool would serve local models; for Claude, the JSON image of the
  form plus our located-error retry loop is what exists.
- **Semantic checking of natural-language specs is research.** ACE reaches first-order logic; turning "the
  export SHALL refuse" into a proof obligation is the model's job, then a test's. Rosette helps only once a
  requirement is already a small formal model.
- **Free-English ambiguity linting has false-positive harm** (note 05 risk): flagging every "should" in a
  README trains the agent to ignore the tool. Lint only inside the controlled field.

## 5. Catalog · G · Controlled language and spec tools

Entry format as in note 02: purpose, in, out (what the model sees), effort (S days / M 1-3 weeks / L month+),
value guess (H/M/L), risks. The shared feedback rule applies: short, located, with a fix.

Racket packages named below (`raco pkg catalog-show`, 2026-09-26):

| package | status here | role |
|---|---|---|
| `parser-tools` (lex, yacc, lex-sre) | in the main distribution; **probe run** [tested]: an EARS sentence parses to `(event-driven (trigger ...) (system ...) (response ...))`, and "The export should write a header row." fails at `line 1 col 37` with the template as the fix | G2 parser; no new dependency, so `raco exe`/`raco distribute` keep working |
| `datalog` | in the distribution; **probe run** [tested]: `covers(T,R) :- requires(K,R), tested(T,R).` answers coverage; `~tested(...)` is a parse error and `_` is refused in rule bodies | G4 facts and cross-task queries; negation in Racket |
| `brag` | catalog, ring 1 (BNF `#lang`, syntax objects with source locations); not run here | alternative G2 parser; a `#lang` needs an installed collection (note 06 §2), so keep it out of the binary |
| `megaparsack` | catalog, ring 1 (parser combinators, always tracks source locations, error → string); not run here | G8 free-text linter, G1 text form |
| `peg` | catalog (PEG parser generator); not run here | a Lojban-style PEG if ever wanted; not planned |
| `rosette` | catalog, ring 1; not installed here | G7 |
| `racklog` | catalog; installed here: not checked, not run | Prolog-style negation if Datalog's absence of `not` bites |
| APE (Attempto) | SWI-Prolog, not Racket; four interfaces (CLI, socket, HTTP, Prolog/Java) [known] | reference semantics for G5; not a dependency |

### G1 · Spec form: a checked s-expression for task specs (M, value M/H)
- In: `(spec name (module m) (when C (then R ...)) (tests ...))` forms in the plan file or stdin. Out: located
  errors (unknown head, wrong arity, unbound name in `(module m)`), or the normalised form plus its English
  rendering.
- Racket: `read-syntax` gives one parse and locations; `syntax/parse` gives the error messages; the renderer is a
  template. This is the cheapest form in §3 and the one closest to what `steer import` already validates.
- Risk: the model has never seen the form; prompt cost of the spec; overlaps with G2, which keeps English.

### G2 · Controlled acceptance criteria: `steer spec check` and criteria on tasks (S-M, value H) ← chosen
- In: one criterion per line in the EARS shapes: `The <system> SHALL <response>.`, `WHEN <trigger>, the
  <system> SHALL <response>.`, `WHILE <state>, ...`, `IF <trigger>, THEN ...`, `WHERE <feature>, ...`; the
  response must name an observable (`writes`, `returns`, `refuses`, `exits with`, `prints`, `contains`, ...).
  Out: the structured form (`--json`), or `error weak-modal criteria.txt:3:12: "should" → write SHALL and say
  what is observable`. On tasks: `#:criteria` in plans, `--criteria` in `add`/`edit`; each criterion is covered by
  a `#:check` (`--covers N`) or marked manual with a reason; `done` refuses while one is uncovered; `show` and
  `resume` print criteria with their covering check.
- Racket: `parser-tools` lexer + LALR grammar (probe above), findings through `common.rkt`, coverage as a Datalog
  export plus a Racket "uncovered" function, renderer for prose. Everything stays in the standalone binary.
- Risk: false rejects of reasonable goals; models game the template ("the code SHALL work"); the lint list must be
  data so it grows from the failure taxonomy. Value depends on the `done` gate, not the grammar.

### G3 · Criteria-to-check scaffolder (M, value M)
- In: parsed criteria. Out: a rackunit (or shell) test skeleton per criterion with a traceability id and a
  `TODO` body, plus the `--check`/`--covers` lines for the task. The model fills bodies; the tool keeps ids
  aligned. Risk: skeletons the model rubber-stamps; measure whether filled tests actually fail before the fix.

### G4 · Requirement traceability over Datalog facts (S, value M)
- In: task store (criteria, checks, anchors) as facts `requires/2`, `tested/2`, `implements/2`. Out: cross-task
  answers (`which tasks cover R`, `orphan checks`) and the uncovered list. Racket: `#lang datalog` for the positive
  queries [tested], plain Racket for negation. Risk: only as good as the `--covers` declarations.

### G5 · ACE-style interpretation for free-text specs (L, value M/unknown)
- In: an English spec restricted to ACE construction rules. Out: DRS-like clauses (entities, relations,
  conditions) with the interpretation rules applied (PP → verb, relative clause → preceding noun, coordination
  precedence), and a paraphrase that shows the reading chosen. Racket: `megaparsack` or `brag` over a
  function-word lexicon; content words are open. Risk: a month of grammar work before the first useful output;
  APE already exists in Prolog; models may not write ACE reliably without constrained decoding.

### G6 · Grammar export for constrained decoding (S-M, value M for local models, L for Claude)
- In: a `parser-tools`/`brag` grammar or the spec form. Out: GBNF for llama.cpp, a JSON schema (structured
  outputs) for the JSON image of the form, and a back-converter. Risk: Claude takes only JSON schemas; token
  choices in the grammar change accuracy (Lost in Space, 2025); duplicates what the located-error retry loop
  already achieves for capable models.

### G7 · Requirement consistency with Rosette (L, value M, research-y)
- In: criteria whose triggers and responses are drawn from a declared finite vocabulary of states and events. Out:
  contradictions (two clauses with the same trigger and incompatible responses) and gaps (a trigger with no
  response) up to a bound, with the witnessing clauses. Risk: needs the vocabulary declared first; `rosette`
  not in the distribution; the same checks are often cheap enumeration without a solver.

### G8 · Ambiguity linter for free English (S, value L/M)
- In: any text field (goal, note). Out: warnings for weak modals, `and/or`, `etc.`, pronouns with several
  candidate antecedents, quantities without units. Racket: `megaparsack` or regexes. Risk: false-positive harm
  (note 05); therefore off by default and never blocking.

### G0 · Esperanto / Lojban / Toki Pona front-ends: not recommended
- Measured 1.44-1.94× tokens on English-centric tokenizers, low LLM exposure, a translation step with no
  checkable intermediate (§3, §4).

## 6. Ranking and the pick

| rank | tool | value | effort | measurable with note 04? | fits `steer`? |
|---|---|---|---|---|---|
| 1 | G2 controlled acceptance criteria + `done` gate | H | S-M | yes: deterministic false-reject rate on real goals, then T5 runs prose vs criteria vs criteria+gate (wrong `done` claims, pass rate, cost per solved task) | yes: `goal` and `check` fields exist, `import` validates, `done` runs checks |
| 2 | G4 traceability over Datalog | M | S | partly (query correctness; agent effect only through G2) | yes, once G2 stores criteria |
| 3 | G1 s-expression spec form | M/H | M | yes, same runs as G2 with the form swapped | yes, but competes with G2 for the same field |
| 4 | G3 criteria-to-check scaffolder | M | M | yes (do filled tests fail before the fix?) | after G2 |
| 5 | G6 grammar export | M/L | S-M | yes for local models only | weak: Claude needs the JSON image |
| 6 | G8 ambiguity linter | L/M | S | false-positive rate only | optional |
| 7 | G5 ACE-style interpretation | M/? | L | hard | no |
| 8 | G7 Rosette consistency | M | L | hard | no |
| 9 | G0 conlang front-ends | none | — | already measured against | no |

**Chosen: G2.** Value per effort: the parser is a day with `parser-tools` (probe done), the tracker fields
slot into the existing store and `import`/`done` paths, and the standalone binary keeps working. Measurability:
the first number (false rejects on this repo's own 27 goals and the exercism statements) needs no model calls;
the second (T5 runs, wrong `done` claims) is note 04's existing design. Fit: it strengthens the tracker's one
rule that already has teeth, "`done` runs the checks", by making the model say what the checks must prove.

Plan: filed in this repo's store as T28-T42 (tag `regular-grammar`; 15 tasks, three milestones) and mirrored to
GitHub as milestones M1-M3 with one issue per task (`steer github sync --tag regular-grammar`).

| milestone | tasks | proof |
|---|---|---|
| M1 · parser, `steer spec check` and `steer spec render` | grammar, lint, render, spec-cmd | end-to-end CLI test, also against `build/steer` |
| M2 · criteria on tasks, coverage, `done` gate, skill text | field, coverage, gate, skill | CLI test: import with criteria → `done` refused → add covering check → `done` |
| M3 · measure and decide (note 04) | corpus, reject-rate, agent-run, decide | `scripts/criteria-eval.rkt status --require`; results in `notes/13-criteria-results.md` |

## Sources to verify

Fetched or searched 2026-09-26; "memory" items were not checked and should be before relying on them.

- Esperanto: Fundamento 16 rules (Don Harlow's commentary, literaturo.org); en.wikipedia "Esperanto grammar",
  "Esperanto Wikipedia" (389 260 articles, 2026-09-19); FLORES-200 language list (`epo_Latn`).
- Lojban: mw.lojban.org "BPFK Section: Formal Grammar", "PEG", "self-segregating morphology",
  "audio-visual isomorphism"; CLL chapter 21 (formal grammars); camxes parsers.
- Toki Pona: tokipona.org, sona.pona.la "Vagueness vs. ambiguity", "Particles"; arXiv 1712.09359; jan-lope
  Toki Pona parser.
- ACE: attempto.ifi.uzh.ch construction and interpretation rules (ACE 6.7); github.com/Attempto/APE (SWI-Prolog);
  Fuchs et al., "Attempto Controlled English for Knowledge Representation", Reasoning Web 2008.
- EARS: alistairmavin.com/ears; en.wikipedia "Easy Approach to Requirements Syntax"; kiro.dev/docs/specs
  (requirements.md in EARS); github/spec-kit issue 1356. Simplified Technical English (ASD-STE100), INCOSE
  requirements-writing rules, QuARS: memory, verify.
- Tokenizers: Petrov et al., "Language Model Tokenizers Introduce Unfairness Between Languages", NeurIPS 2023
  (arXiv 2305.15425); OPUS Tatoeba v2023-04-12 (Tiedemann 2012); Anthropic token-counting reference
  (`count_tokens`; tiktoken undercounts Claude) and pricing page
  (platform.claude.com/docs/en/about-claude/pricing.md) for the input/output ratio.
- Reasoning across languages: Shi et al., MGSM (arXiv 2210.03057); MGSM-Rev2 / MGSM-Pro (arXiv 2601.21225);
  Wendler et al., "Do Llamas Work in English?", ACL 2024 (arXiv 2402.10588); "Mythbuster: Chinese Language Is
  Not More Efficient Than English in Vibe Coding" (arXiv 2604.14210).
- Constrained decoding and formats: llama.cpp GBNF; XGrammar (arXiv 2411.15100); "Lost in Space" (arXiv
  2502.14969); TOON vs JSON benchmark (arXiv 2603.03306); Claude structured outputs
  (platform.claude.com/docs/en/build-with-claude/structured-outputs.md).
- Gherkin with LLM agents: arXiv 2607.01980; ACM A-TEST 2024 (doi 10.1145/3678719.3685692).
- Racket: docs.racket-lang.org/brag, /megaparsack, /parser-tools, /datalog; `raco pkg catalog-show` for brag,
  megaparsack, peg, rosette, parsack, ragg (legacy), racket-langserver.
