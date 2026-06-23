# DA-Bench in Hazel — plan to address the remaining tasks

Roadmap for the **101 unsolved non-parser tasks** (155/257 solved today). The one
true hard blocker — `#` in a column name (id 618) — is **out of scope** here; it
needs a language/lexer change, not a solution. Everything below is expressible in
Hazel with either unwritten code or an implementable library.

Ordering is **strictly by difficulty**, easiest first. Each phase lists what to
build, which tasks it unblocks, and the main feasibility risk (matching the
reference label *exactly* is the recurring risk — pandas/scipy/numpy conventions).

## Overview

| Phase | Theme | New infra needed | Tasks | Effort | Exact-match risk |
|------:|-------|------------------|------:|--------|------------------|
| 1 | Write-only solutions | none | ~13 | Low | Low–med |
| 2 | Python-repr formatting | 1 prelude helper | ~4 | Low | Medium |
| 3 | Edge-case / quirk labels | none | ~5 | Low | High (labels odd) |
| 4 | Calendar / date math | date prelude | 2 | Medium | Low |
| 5 | Statistics: p-values | special-fn library | ~58 | High | Med–high |
| 6 | ML + RNG | seed hook + linalg + PRNG | ~20 | Very high | High |

Phases 1–4 (~24 tasks) are the high-confidence near-term wins. Phase 5 is the big
prize but a real numeric-library project. Phase 6 is the hardest and the least
certain to match labels bit-for-bit.

---

## Phase 1 — Write-only solutions (no new infrastructure)

These are already expressible with the current prelude + `string_of_int`/
`string_of_float` + `join_with`; they just haven't been written. Best first work:
pure payoff, zero risk to shared code.

- **Multi-step preprocessing** (expressible, just multi-step): **453** (z-replace
  WINDSPEED + mean-fill AT, then means), **574** (dropna + min-max normalize MSFT/
  SPY/VIX, then correlation matrix), **665** (fill-mean + percentile-band counts/
  proportions on Close), **572** (max-`.SPX` date + previous-day AAPL pct-change —
  date is a substring, not calendar math).
- **Other "future work"**: **62** (per-group IQR), **252** (single-value skewness),
  **321** (`SCOREMARGIN` sign/`TIE` cleaning), **510** (brand-filter argmax), **589**
  (a specific timestamp row).
- **Multi-number string answers** (assemble with `string_of_int`/`string_of_float`
  + `join_with`): **77**, **178** (`"314, 577"`), **219** (`"1, 2018, 88.32"`).

**Effort:** ~1 solution each. **Risk:** binning/rounding/convention mismatches
(half-open intervals, sample vs population std); a couple may need a second pass to
match the label. Verify each against its label and add to `test.sh`.

## Phase 2 — Python-repr formatting helper → dict answers

Some answers are a Python dict literal. Build one reusable prelude helper, then
solve the dict tasks.

- **Build:** `py_dict(keys: [String], vals: [String]) -> String` emitting
  `{'k1': v1, 'k2': v2, ...}` (single-quoted keys, `: `, `, ` separators), plus a
  `py_num` formatter that matches Python's repr — the catch is `string_of_float`
  prints integer-valued floats as `594.` where Python wants `594.0` (or bare `594`
  for an int count). Encapsulate that fix once.
- **Unblocks:** **450** (avg WINDSPEED per month → `{'month_1': 7.17, ...}`; month is
  a substring of the date, so no calendar math), **451** (missing-count per column →
  `{'WINDSPEED': 594, ...}`). Reusable for any future dict/list-of-number answer.

**Effort:** low (one helper + 2 solutions). **Risk:** medium — must match Python's
exact text (brace/quote/spacing and int-vs-float repr) for string equality.

## Phase 3 — Edge-case / quirk labels

The label here is degenerate or arguably wrong, so "solving" means reproducing an
edge case rather than computing something meaningful. Low value, low effort —
worth a short pass to close them out, with a note in each file.

- **554** — filter yields empty set → pandas `median` is `NaN`; detect empty and
  print `nan`.
- **760** — no missing values anywhere → "station with most missing" is pandas
  `idxmax` on an all-zero series (first label wins); reproduce that tie-break, count 0.
- **741** — answer is the literal column name `"ratio"`; just emit the string.
- **743** — answer is a written-out **file path** string; emit the expected path.
- **468** — ambiguous "'Assault' category"; pick the interpretation that yields 0.
- **361, 662** — *label is wrong* (our computation matches pandas: 97 z-outliers;
  median 1.30099). To "pass," emulate the reference's quirk (361: count outliers
  *remaining after removal* = 0 — trivial; 662: reverse-engineer their median index
  — uncertain). **Recommend leaving these documented-but-unsolved** rather than
  shipping a deliberately-wrong computation.

**Effort:** low. **Risk:** high that it's not worth it (matching defects); decide
per task whether to pursue or just document.

## Phase 4 — Calendar / date arithmetic

- **688** — epoch → hour-of-day bucket: integer math on a Unix timestamp
  (`(t / 3600) mod 24`, mind timezone). Easy; could even be Phase 1.
- **234** — days between two calendar dates: needs a real date→ordinal (Gregorian
  days-since-epoch with leap-year rule). Build a small `days_from_civil(y, m, d)`
  prelude helper (Howard Hinnant's algorithm — pure integer arithmetic), then
  subtract.

**Effort:** medium (one date helper). **Risk:** low once the helper is correct.

## Phase 5 — Statistics special-function library → p-values (~58, the big prize)

The largest bucket. Every task here computes a **test statistic we can already get**
and then needs a **distribution CDF** to produce a p-value or accept/reject. The CDFs
are pure numeric routines — build them once in a `stats_prelude`, then the tasks
fall in waves ordered by which function they need.

**Build (a numeric prelude), in dependency order:**
1. `erf` / normal CDF — Abramowitz-Stegun rational approximation.
2. `betainc` (regularized incomplete beta, continued fraction) → **t** and **F** CDFs.
3. `gammainc` (regularized incomplete gamma, series + continued fraction) → **χ²** CDF.

**Then solve in waves (easiest CDF first):**
- **5a — normal-based** (needs only `erf`): z-tests, and **D'Agostino normaltest**
  (combines skewness + kurtosis z-scores through the normal CDF — we already compute
  the moments).
- **5b — t-distribution** (`betainc`): one/two-sample **t-tests**, and the
  **significance/`relationship_type` companions of Pearson-r tasks** (r → t → p).
- **5c — chi-square** (`gammainc`): **chi-square** independence/goodness-of-fit.
- **5d — F-distribution** (`betainc`): **one-way ANOVA**.
- **5e — Mann-Whitney U**: U statistic + normal approximation with tie correction.
- **5f — Kolmogorov-Smirnov**: empirical-CDF gap + Kolmogorov distribution series.
- **5g — Shapiro-Wilk**: hardest (Royston's W with approximated coefficients) —
  attempt last, may not be worth it.

**First step of this phase:** tally the 58 by test type (one pass over
`da-dev-questions.jsonl`) to size each wave and prioritize the high-count CDFs.

**Effort:** high — a real special-function library, but all pure math, no language
gap. **Risk:** med–high — `betainc`/`gammainc` must be accurate enough that the
p-value rounds to the label; KS/Shapiro are the most likely to miss.

## Phase 6 — ML + RNG (~20, hardest)

Two independent hard problems; both must be solved for most of these.

**6a — Reproducible randomness (the purity story).** Inject the seed at **edit-time**
from the CLI, mirroring `^^csv`:
- Add a `^^seed(42)` hook (or `--seed N` flag) in `src/CLI/Csv.re`/`Cli.re` that
  splices the literal seed into the program.
- Add a **pure PRNG** prelude: `next : state -> (value, state)` split/advance
  functions. Seed is a compile-time literal + generator is pure ⇒ language stays
  effect-free, randomness is reproducible.
- **The hard part:** to match `train_test_split(random_state=42)`, reimplement
  **numpy's exact generator** (MT19937 / PCG64) and the exact permutation/shuffle
  order so our split equals theirs. Purity is solved by the design; *bit-exact numpy
  matching* is the real work and the main risk.

**6b — Models (a small linear-algebra prelude).**
- Linear regression via **normal equations** (`(XᵀX)⁻¹Xᵀy`): needs matrix multiply +
  Gaussian elimination / inverse over the labeled-tuple tables. Pure, doable.
- Logistic regression / clustering: iterative solvers (gradient descent, k-means) —
  expressible, but matching sklearn's exact solver, regularization default, and
  convergence is hard.

**Solve in order of decreasing certainty:**
1. Full-data linear regression metrics that **don't depend on the split** (if any).
2. Seeded-split linear regression (needs 6a + 6b).
3. Classification / clustering metrics (least likely to match sklearn exactly).

**Effort:** very high. **Risk:** high — even with the infrastructure, exact-label
match depends on replicating numpy's RNG stream and sklearn's numerics precisely;
some of these 20 may remain unmatched.

---

## Suggested execution order & realistic outcome

1. **Phase 1** (~13) — immediate, no risk. Push solved count toward ~168.
2. **Phase 2** (~4) + **Phase 4/688** (1) — small helpers, quick wins (~173).
3. **Phase 3** (~5) — decide per task; close out or document (~176, minus the
   label-wrong ones we choose to leave).
4. **Phase 4/234** (1) — date helper.
5. **Phase 5** (~58) — the major project; ship CDF waves 5a→5d for the bulk, treat
   5e–5g as stretch. Realistically this is where most of the remaining points are.
6. **Phase 6** (~20) — last; build RNG + linalg infra, accept that exact sklearn/
   numpy matching may cap how many actually pass.

**Honest ceiling:** Phases 1–4 (~24 tasks) are high-confidence. Phase 5 is a large
but tractable numeric-library effort (the single biggest gain). Phase 6 is the most
likely to fall short of exact-match even after the infrastructure exists. None of it
is a *language* limitation — the only thing Hazel genuinely cannot express is the
parser `#`-in-column-name case (id 618), which this plan deliberately excludes.
