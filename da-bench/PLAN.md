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

## Phase 1 — Write-only solutions (no new infrastructure) — DONE (11 solved, 1 defect)

Already expressible with the current prelude + `string_of_int`/`string_of_float` +
`join_with`. All written, verified against labels, and added to `test.sh`:

- **Multi-step preprocessing** ✅: **453** (z-replace WINDSPEED + mean-fill AT),
  **574** (3-col dropna + correlation matrix; min-max scaling is correlation-
  invariant), **665** (fill-mean + percentile bands), **572** (max-`.SPX` date +
  previous-row AAPL pct-change — used a flat 5-tuple fold accumulator; a nested-tuple
  accumulator left the result indeterminate).
- **Other "future work"** ✅: **62** (IQR-trimmed mean), **321** (`SCOREMARGIN`
  TIE→0 / signed parse → IQR count), **510** (brand-filter argmax over `distinct_strings`),
  **589** (timestamp stored as `Apr 13  2017 8:00:00 AM`, not `20170413_080000` — found
  by the human-readable form).
- **Multi-number string answers** ✅: **77** (`"1, 2018, 88.32"` — month-name parse
  on `DD-Mon-YY`), **178** (`"314, 577"` / `"0, 1, 0.0629"`), **219** (`"9.03,9.0"`).

**Caveat — 252 is a label defect, not a Phase-1 win** (reason found during
implementation, like 361/662): "highest-skewness country" — skewness across each
country's year series peaks at **Myanmar** (adjusted and biased agree; Afghanistan
ranks ~24/33). The label `Afghanistan` is the degenerate case where skewness of the
*single* 1992 value is `NaN` for every country and pandas `idxmax` returns the first
row. `da252-gapminder-skew.hz` emits the honest answer and documents this; it is
**excluded from `test.sh`**, not counted as solved.

Formatting note: `string_of_float` prints integer-valued floats as `594.`; where an
answer needs `0`/`1` (e.g. 178's scaled min/max) use `int_of_float`, and for a value
like `9.0` (219) a tiny local helper appends a `0` after a trailing dot. No shared
infra was added.

## Phase 2 — Python-repr formatting helper → dict answers — DONE (2 solved)

Built three reusable `prelude.hz` helpers and solved the dict tasks:

- **`py_dict(keys, vals)`** — parallel key/value-string lists → `{'k1': v1, ...}`
  (single-quoted keys, `: `, `, ` separators). Best when keys are dynamic.
- **`dict_of_tuple(show_val, t)`** — a labeled tuple → dict via `to_lvs`, with a
  per-value formatter (e.g. `dict_of_tuple(string_of_int, (a=1, b=2))` = `{'a': 1, 'b': 2}`).
- **`py_float(x)`** — Python-repr float: appends `0` after a bare trailing dot, since
  `string_of_float(6.0)` is `6.` where Python wants `6.0`.

Solved ✅: **450** (avg WINDSPEED per month → `{'month_1': 7.17, ...}`; `DATE TIME` is
MM/DD/YYYY, month = field 0), **451** (missing-count per column → `{'DATE TIME': 0,
'WINDSPEED': 594, ...}`). Both verified against the labels and in `test.sh`.

## Phase 3 — Edge-case / quirk labels — DONE (3 solved, 4 documented defects)

On inspection these split into legitimately-computable edge cases and genuine
defects. Solved the former; documented the latter (file with a note, not in `test.sh`).

Solved ✅:
- **468** — IQR outliers of Age in the 'Assault' category: a real computation; the
  filter is `Offense` containing "Assault" (681 rows; there is no Category "Assault") → 0.
- **554** — median HT_M where CON=1 & PLTID=5: the filter is empty, and median-of-empty
  = `nan` is the *correct* result, not a defect. Emit `"nan"`.
- **760** — most-missing station: all counts are 0, so the answer is pandas `idxmax`'s
  deterministic tie-break = the alphabetically-first station; reproduced via a sorted
  argmax over `distinct_strings` (`AGE00135039`, 0).

Documented defects (not solved — emulating them adds nothing):
- **741** — graded value is the literal column name `"ratio"`, not a computed value
  (Class G). `da741` builds the honest Balance/Limit feature.
- **743** — graded answer includes a written-out file path; Hazel is pure and can't
  write files (Class D). `da743` computes the honest Income min/max.
- **361, 662** — *label is wrong* (our computation matches pandas: 97 z-outliers; median
  1.30099). Left documented rather than shipping a deliberately-wrong computation.

## Phase 4 — Calendar / date arithmetic — DONE (2 solved)

- **688** ✅ — epoch → time-of-day buckets via `(dt / 3600) mod 24` (UTC and local
  agree on this data; counts 6/6/6/6).
- **234** ✅ — mean budget-year duration: M/D/YYYY dates → days-since-epoch via an
  inline Howard-Hinnant `days_civil(y, m, d)` (pure integer arithmetic), subtract,
  average → 364. The date helper is local to `da234` (only task that needs it; promote
  to the prelude if a future task wants date math).

## Phase 5 — Statistics special-function library → p-values (~58, IN PROGRESS)

The largest bucket. Every task computes a **test statistic we can already get** and
then needs a **distribution CDF** for the p-value/decision. The CDFs are pure numerics,
built once in `prelude.hz`.

**Library — BUILT & verified against scipy/known values:** `erf`, `normal_cdf`,
`lgamma` (Lanczos), `betacf`/`betainc` (incomplete beta, Lentz), `t_sf2` (two-sided
Student-t), `pearson_p`, `ttest_ind_p` (pooled), `ttest_welch_p` (unequal-var),
`normaltest_p` (D'Agostino-Pearson), plus `days_civil`/`monthnum` for date parsing.
Still to add: `gammainc` (→ χ²) and an F-distribution helper.

**Done so far (13):** Pearson-r p-value / relationship — **11, 34, 66, 140, 326, 408,
452, 668**; two-sample t-tests — **109, 415** (pooled), **419** (Welch — its label used
`equal_var=False`); D'Agostino normaltest — **652, 729**. All exact. Note the
`relationship_type`/significance classification keys off p<0.05, so `pearson_p` also
unlocks corr tasks whose label omits the p-value. (id 297 deferred: a pandas
CSV-parsing discrepancy on the Newick `tree_table.csv`, not a p-value issue.)

**Remaining build, in dependency order:**
1. (done) `erf` / normal CDF — Abramowitz-Stegun.
2. (done) `betainc` → **t** and **F** CDFs.
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

1. **Phase 1** — DONE: 11 solved (155 → **166**), 252 documented as a label defect.
2. **Phase 2** — DONE: 2 solved (450, 451 → **168**) via the dict helpers.
3. **Phase 3** — DONE: 3 solved (468, 554, 760 → **171**); 741/743/361/662 documented.
4. **Phase 4** — DONE: 2 solved (234, 688 → **173**); Class C calendar cleared.
5. **Phase 5** — IN PROGRESS: library built & verified; corr p-values, t-tests (pooled
   & Welch), and D'Agostino normaltest done (13 tasks → **186**). Remaining: χ² + ANOVA
   (need `gammainc`/F), KS, Mann-Whitney, and the big Shapiro-Wilk bucket (~20, hardest).
6. **Phase 6** (~20) — last; build RNG + linalg infra, accept that exact sklearn/
   numpy matching may cap how many actually pass.

**Phases 1–4 are complete (155 → 173).** What remains is the two hard library
efforts (Phase 5 p-values, Phase 6 ML+RNG) plus the documented defects/parser blocker.

**Honest ceiling:** Phases 1–4 (~24 tasks) are high-confidence. Phase 5 is a large
but tractable numeric-library effort (the single biggest gain). Phase 6 is the most
likely to fall short of exact-match even after the infrastructure exists. None of it
is a *language* limitation — the only thing Hazel genuinely cannot express is the
parser `#`-in-column-name case (id 618), which this plan deliberately excludes.
