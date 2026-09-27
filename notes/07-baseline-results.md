# 07 · Baseline results (T9) + a first "does steering help" read (cheap proxy for T15)

Purpose: the fastest honest answer to "does steering (steer's tools) actually improve pass rate",
using the harness that already exists, before investing in the fuller pipeline (note 04 T11-T15).
This is a first read on a small slice, not the final study — see "how much this settles" at the end.

## Setup

- Model: `mlx-community/Qwen3.5-9B-MLX-4bit`, loaded via `mlx_lm` (Apple MLX), greedy decoding
  (`temperature=0.0`), `max_tokens=1500` per turn. Offline, already cached locally.
- `.venv-mlx` did not exist in this checkout; created with `uv venv .venv-mlx --python 3.11` +
  `uv pip install mlx-lm` (mlx-lm 0.31.3, mlx 0.32.2) so the harness in `scripts/mlx_generate.py`
  could run as documented.
- `samples/` was gitignored and empty; ran `racket scripts/samples.rkt fetch` then `build` per
  `Makefile`'s `samples` target (network access was available). Task-set sizes matched note 04/T8:
  eval/tasks T1 498, T2 37, T3 126.
- Task slice (seed `baseline1`, fixed across all conditions so C0/C1/C2-lite score the *same* tasks):
  **25 T1** (Exercism exercises, write-from-scratch) + **40 T3** (repair: 20 deleted-paren mutants,
  20 unbound-name mutants) = **65 tasks**. T2 and MultiPL-E were skipped for this pass (out of scope
  for the steering-tool comparison; T1+T3 is what note 04's A1-adjacent tools bear on).
- Conditions:
  - **C0**: single attempt, no feedback (note 04's baseline).
  - **C1**: on any C0 failure, one retry showing the model the raw `raco make`/`raco test` output
    (what an agent looking at its own compiler would see today).
  - **C2-lite**: on any C0 failure *whose failure `steer syntax` actually flags as a structural
    (bracket/paren) error* — see "eligibility" below — one retry showing the model `steer syntax
    FILE --fix`'s located-error-plus-verified-repair report instead of the raw error. Chose `--fix`
    (not plain `steer syntax`) because the verified repair is exactly what A1's own strong prior
    evidence (XL1: 41/41 Python, 21/21 C# seeded bracket errors detected, most with a verified
    repair) is claiming credit for; showing the diagnosis without the repair would understate it.
- All three conditions share the same turn-1 completions (C0 IS turn 1); C1/C2-lite differ only in
  what turn 2 sees. This is the harness extension for this task; not yet a tracked `.steer` task.

## Harness bug found and fixed while doing this for real

`scripts/eval-local.rkt`'s scorer had only ever been run against synthetic reference/stub/wrong-answer
fixtures. Running it on real model output surfaced a real bug: the scorer's error-detail text (used
to build C1's "raw error" retry prompt) was built with `tail-text` — the **last** ~20 lines/1500 chars
of `raco make`/`raco test` output. For both tools, the actually useful message is at the **front**:

- `raco make`'s read/compile errors print the real message on line 1, then a long
  `context...` stack-trace dump (Racket's internal compiler frames) — tail-text kept only the stack
  dump and threw away the message.
- `raco test` prints each failure's `name:`/`location:`/`params:` at the point it happens, then
  repeats aggregate `N success(es)/failure(s)/error(s)` counts afterward — tail-text kept only the
  repeated counts.

Concretely, before the fix, the C1 feedback for `exercism-bowling` was 20 lines of
`/Applications/Racket v9.3/collects/...` stack frames with no error message at all. Fixed in
`scripts/eval-local.rkt` by adding `head-text` (keeps the front, and for `raco make` output stops at
the `context...` marker so the stack dump is never included) and using it instead of `tail-text` for
both `no-compile` and `wrong` detail. Re-scored turn 1 after the fix (pass rates unchanged, since the
fix only touches the `detail` field, not pass/fail) and regenerated C1's turn-2 prompts +
completions from the corrected feedback before the numbers below. This does not affect C2-lite,
whose feedback comes from `steer syntax` directly, not from the judge's `detail` field.

This is worth fixing upstream regardless of this eval: any future condition that shows a model "the
compiler error" (C1, C2, C3, …) was going to get this same corrupted feedback.

## Results

### Overall pass rate (all 65 tasks; Wilson 95% CI)

| condition | n | pass | 95% CI | no-compile | wrong | timeout | total completion tokens | total wall time |
|---|---|---|---|---|---|---|---|---|
| C0 (single attempt) | 65 | 23% | 15–35% | 43 | 7 | 0 | 26,784 | 26.0 min |
| C1 (raw-error retry) | 65 | 29% | 20–41% | 33 | 11 | 2 | 48,904 | 49.1 min |
| C2-lite (steer-syntax retry) | 65 | 23% | 15–35% | 43 | 7 | 0 | 42,524 | 40.2 min |

(C2-lite's no-compile/wrong/timeout counts equal C0's because none of its 28 retried tasks changed
outcome — see below.)

By task set (turn count = 1 unless retried):

| set | n | C0 pass | C1 pass | C2-lite pass |
|---|---|---|---|---|
| T1 exercises | 25 | 20% (9–39%) | 24% (11–43%) | 20% (9–39%) |
| T3 deleted-paren | 20 | 40% (22–61%) | 45% (26–66%) | 40% (22–61%) |
| T3 unbound-name | 20 | 10% (3–30%) | 20% (8–42%) | 10% (3–30%) |
| all | 65 | 23% (15–35%) | 29% (20–41%) | 23% (15–35%) |

C1 recovered 4 of the 50 C0 failures (2 T1, 1 paren, 2 unbound — one T1 task's raw-error retry
actually regressed the T1 unbound count is a wash; net +4 passes across the slice). C2-lite recovered
0 of its 28 eligible retries.

### Eligibility for C2-lite

Of the 50 C0 failures, `steer syntax --json` on the failing completion reported at least one
`severity: error` finding (i.e., a genuine bracket/read structural error, not an unbound-id or
semantic failure `steer syntax` has no way to see) for **28/50 (56%)**. Those 28 are the only fair
test of C2-lite; the other 22 failures (mostly `unbound identifier`, or code that reads fine but
fails a test) are outside what `steer syntax` can address at all, and C2-lite correctly leaves them
un-retried rather than wasting a turn.

### Head-to-head on the syntax-error subset (the fair comparison)

| condition | n | pass | 95% CI |
|---|---|---|---|
| C1 (raw error retry) | 28 | 0% | 0–12% |
| C2-lite (steer syntax retry) | 28 | 0% | 0–12% |

Both conditions recovered **zero** of the 28 tasks whose C0 failure was a genuine structural error,
whether shown the raw compiler error or `steer syntax --fix`'s verified repair.

### Why: inspected several C2-lite non-recoveries directly

For at least one task (`exercism-bowling`), the turn-2 completion was **byte-identical** to turn 1:
the model regenerated the exact same (truncated, repetitive) output verbatim, ignoring the retry
feedback entirely. Checked whether this is common: 5 of 43 C0 no-compile completions hit or nearly
hit the 1500-token generation cap (`completion_tokens` ≥ 1400), i.e. the "syntax error" was actually
mid-expression truncation from running out of budget, not a near-miss bracket bug — no amount of
located-error feedback fixes that. This affects at most 5/28 of the eligible subset and hits C1 and
C2-lite equally, so it doesn't bias the head-to-head, but it means the true "can feedback fix a
one-bracket mistake" signal is diluted by a `max_tokens` limitation of this harness run, worth
raising before spending more slices on this model at this token budget.

## Applying note 04's decision rule

> Keep a tool if it improves pass@k by a margin above noise on T1-T3 without raising false-positive
> harm. Drop or redesign a tool whose feedback is followed but does not raise pass rate.

On this slice: **C2-lite shows no measurable improvement over C0 on the one subset where it could
possibly help** (0/28 vs C0's own 0/28 on those same tasks, and 0/28 vs C1's 0/28). It is not "raising
false-positive harm" either — it just isn't moving the needle. C1 (raw error) *did* move the overall
number (+4 tasks, 23%→29%), which is itself informative: **the model can sometimes use feedback to
fix things when given the actual compiler output**, so the mechanism of "retry with feedback" is not
dead — but the specific tool tested here (`steer syntax --fix` on top of an on-the-fly Qwen3.5-9B
completion) contributed none of that gain on this slice.

**Plain verdict: this one data point does not clear the bar to justify building T11-T15 (A2/A3,
sandboxed executor) on the strength of A1 alone.** The evidence for "steering helps" here comes from
generic retry-with-compiler-error (C1), not from steer's own tool (C2-lite). That is a real,
if modest and noisy, signal that a fuller retry-loop study (C2-C4 with A2/A3 once built) is worth
running — but it does not yet show that the *specific* tool built so far (A1/`steer syntax`) is
pulling weight beyond what a bare compiler error already gives an agent.

**Is 28 tasks (or 65 overall) enough to tell?** No — say this plainly, not rounded up. The 95% CI on
the subset is 0–12%: a true C2-lite recovery rate as high as ~10-12% is fully consistent with what we
saw (0/28), and so is a true rate of ~0%. The overall C0-vs-C1 CIs (15–35% vs 20–41%) overlap by most
of their width, so even the "C1 helps" reading of this data is suggestive, not conclusive, at n=65.
**A next slice would need roughly 150-250 eligible syntax-error tasks (not 28) to distinguish a
real ~10-15pp C2-lite effect from zero at this pass-rate range with reasonable power**, which given
~56% of C0 T3/T1 failures are syntax-eligible means running C0 on roughly 300-450 T1+T3 tasks total
(vs. the 65 run here) — well short of the full 661-task non-holdout set, but several times this pilot.

## What this run does and doesn't tell us about note 04's other questions

- Q1 (does tool feedback raise pass rate): weak yes for generic retry-with-error (C1); no signal yet
  either way for steer's specific tool (C2-lite, too few eligible tasks).
- Q3 (cost): both retries roughly double tokens/time per task attempted (C1: ~1.8x tokens, C2-lite:
  ~1.6x, mostly from the ~5 truncation-affected tasks re-spending their full 1500-token budget again).
- Q4 (remaining failure taxonomy): unbound-identifier failures (T3 unbound-name: 10% pass at C0,
  20% at C1, still the weakest set) are the largest remaining class this pilot can see, and they are
  exactly what A1 cannot touch — consistent with note 04's prioritization of A2 (binding/arity
  checker) as the next tool to build.

## Reproducing

```
racket scripts/eval-local.rkt prompts run1/prompts.jsonl --seed baseline1 --multipl 0 --t1 25 --t2 0 --t3 40
.venv-mlx/bin/python scripts/mlx_generate.py run1/prompts.jsonl run1/completions1.jsonl
racket scripts/eval-local.rkt score run1/prompts.jsonl run1/completions1.jsonl run1/results1.jsonl
racket scripts/retry-eval.rkt eligibility run1/results1.jsonl run1/elig.jsonl
racket scripts/retry-eval.rkt turn2-prompts c1     run1/prompts.jsonl run1/results1.jsonl run1/elig.jsonl run1/c1-turn2-prompts.jsonl
racket scripts/retry-eval.rkt turn2-prompts c2lite run1/prompts.jsonl run1/results1.jsonl run1/elig.jsonl run1/c2lite-turn2-prompts.jsonl
.venv-mlx/bin/python scripts/mlx_generate.py run1/c1-turn2-prompts.jsonl run1/c1-completions2.jsonl
.venv-mlx/bin/python scripts/mlx_generate.py run1/c2lite-turn2-prompts.jsonl run1/c2lite-completions2.jsonl
racket scripts/eval-local.rkt score run1/c1-turn2-prompts.jsonl run1/c1-completions2.jsonl run1/c1-results2.jsonl
racket scripts/eval-local.rkt score run1/c2lite-turn2-prompts.jsonl run1/c2lite-completions2.jsonl run1/c2lite-results2.jsonl
racket scripts/retry-eval.rkt combine run1/results1.jsonl run1/c1-results2.jsonl run1/c1-combined.jsonl
racket scripts/retry-eval.rkt combine run1/results1.jsonl run1/c2lite-results2.jsonl run1/c2lite-combined.jsonl
racket scripts/eval-local.rkt report run1/c1-combined.jsonl
racket scripts/eval-local.rkt report run1/c2lite-combined.jsonl
racket scripts/retry-eval.rkt subset-report run1/elig.jsonl run1/c1-combined.jsonl run1/c2lite-combined.jsonl
```

`run1/*.jsonl` (prompts/completions/results) are the exact artifacts behind the numbers above;
gitignored like the rest of `samples/`/generated eval data, so this file is the durable record.
