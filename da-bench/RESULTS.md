# DA-Bench in Hazel — results ledger

Tracking the attempt to express **every** InfiAgent DA-Bench dev task (257 total) in
Hazel. This file records what passes, what fails, and — most importantly — the
**classes of issues** that make a task inexpressible or unrunnable.

Status counts are updated as waves land; see `test.sh` for the authoritative
list of passing cases (every entry there is verified against the InfiAgent label).

## Classes of inexpressibility / failure

These are the recurring reasons a task can't be solved (as opposed to a one-off
data-cleaning wrinkle):

- **(A) scipy hypothesis-test p-values** — Shapiro-Wilk, Kolmogorov-Smirnov,
  `normaltest`, t-test, ANOVA, etc. Need statistical distribution CDFs / special
  functions we have no library for. A *coefficient* (Pearson r, skewness, kurtosis)
  is fine; the p-value / accept-reject half is not. ~58 tasks flagged.
  - Caveat: some "normality" tasks are gated on **skewness bounds** (e.g. |skew|<0.5),
    NOT on a real test — those ARE expressible (ids 19, 25, 359, 465, 222, 337, 593...).
- **(B) sklearn / ML models** — `train_test_split(random_state=42)` (needs numpy RNG),
  fitted regression/classification models, clustering. ~22 tasks flagged.
- **(C) true calendar arithmetic** — days between two calendar dates (id 234),
  month/year extraction from real date strings (77, 450, 572). Substring/`HH:MM:SS`→
  seconds (587, 593) IS expressible via string_split; only calendar-aware date math is not.
- **(D) non-value answers** — the graded answer is a **file path** or an emitted CSV
  artifact (id 743 income_normalization path, 220 cleaned_dataset). No value to compute.
- **(E) evaluator stack-overflow — FIXED.** Large columns used to overflow because
  three traversals on the run path grew the (small, ~1 MB) js_of_ocaml/node stack one
  frame per row: `StaticsBase.map_m`/`map_m2` (which also did `xs @ [x]`, O(n²)),
  `Statics`' `ListLit` case, and `ValueChecker.req_all_final`. These are now
  tail-recursive (see `src/util/ListUtil.re` helpers + the three modules), so
  sort/median/quantile/correlation run on the full tables: tree (9796), weather_train
  (16683), baro_2015 (8736), veracruz (8760), arrest (8638), weather_data_1864 (5686),
  YAHOO-BTC (2175), tr_eikon (2216) all evaluate without overflow. 20 of the former
  class-E tasks are now solved (folded into the verified count) — including 123 and 555,
  which need distinct-value dedup: the O(n²) list-membership `distinct` was too slow even
  once it stopped overflowing, so `prelude.hz` adds `distinct_strings` (a recursive
  `insert_sorted_uniq` insertion sort over `string_compare`, keeping the accumulator at
  distinct-count size). Two tasks have correct computations but disagree with their labels
  (361: 97 outliers vs label 0, which counts outliers *remaining* after removal; 662:
  median 1.30099→1.30 per statistics.median vs label 1.31). The rest are unsolved for
  non-overflow reasons: Python-dict answer formatting (450, 451), ambiguous/degenerate
  specs (468, 554, 760), and multi-step tasks left as future work (453, 572, 574, 665).
- **(F) CSV ingestion gaps** — e.g. a UTF-8 **BOM** on the first column header makes
  that column's label `﻿<name>`, so `data.`name`` projection fails. Affects the
  first column of `beauty and the labor market.csv` (wage) and `gapminder_cleaned.csv`
  (year). Fixable by stripping the BOM during ingestion in `src/CLI/Csv.re`.
- **(G) degenerate/odd grading** — e.g. id 554 expects literal `nan` (median of an
  empty filtered set); id 741 grades the literal column name `"ratio"`. Not a
  computation in the usual sense.

## Tally (257 dev tasks) — 186 solved, 71 accounted for

- **Solved & verified — 186** (in `test.sh`, every output matches the InfiAgent label):
  mean/median/std (sample & population)/min/max/range, Pearson correlation,
  skewness (pandas-adjusted AND scipy-biased), kurtosis, IQR & Z-score outlier
  detection/removal, group-by aggregation, feature engineering (ratios, sums,
  pct-change, log/log10 transforms, min-max normalize, label-encode), missing-value
  counts/percentages, argmax (numeric and over columns), filtered medians, title/string
  extraction, comma-separated-name list answers, HH:MM:SS→seconds time parsing,
  multi-step preprocessing (id 28, 271), and conditional/banded correlations.
  Includes 20 large-table tasks unblocked by the tail-recursion fix (class E above):
  277, 278, 282, 359, 360, 446, 447, 465, 466, 551, 552, 553, 657, 659, 663, 755, 757, 759,
  plus 123 and 555 via a new sorted-insertion `distinct_strings` (low-cardinality dedup that
  the O(n²) `distinct` could not finish — uses `string_compare` + a recursive `insert_sorted_uniq`).
  Plus 11 PLAN.md Phase-1 tasks (write-only, no new infra): 62, 77, 178, 219, 321, 453, 510,
  572, 574, 589, 665 — multi-step preprocessing, IQR/argmax over groups, and multi-number
  string answers assembled with `string_of_int`/`string_of_float` + `join_with`.
  Plus 2 PLAN.md Phase-2 tasks (450, 451) — Python-dict answers via new `py_dict` /
  `dict_of_tuple` / `py_float` prelude helpers.
  Plus 3 PLAN.md Phase-3 tasks (468 IQR over an 'Assault'-filtered subset, 554
  median-of-empty → "nan", 760 most-missing-station via sorted argmax) — the
  legitimately-computable edge cases.
  Plus 2 PLAN.md Phase-4 tasks (234 mean budget-year duration via a Gregorian
  days-from-civil helper, 688 epoch→time-of-day buckets via `(dt/3600) mod 24`).
  Plus PLAN.md Phase-5 (in progress, 13 tasks) via a new pure special-function prelude
  (`erf`/`normal_cdf`, `lgamma`, `betacf`/`betainc`, `t_sf2`, `pearson_p`, `ttest_ind_p`,
  `ttest_welch_p`, `normaltest_p`), all verified against scipy/known values:
  Pearson-r p-value / relationship (11, 34, 66, 140, 326, 408, 452, 668), two-sample
  t-tests (109, 415 pooled; 419 Welch — its label used equal_var=False), and D'Agostino
  normaltest (652, 729).
- **scipy p-value / hypothesis tests (class A): 58 → 45 (Phase 5 in progress).** The
  special-function prelude now does Pearson-r p-values, pooled & Welch t-tests, and
  D'Agostino normaltest exactly (13 solved). Remaining: Shapiro-Wilk (~20, hardest),
  chi-square + ANOVA (need `gammainc` / F), KS, Mann-Whitney, and a few more corr/t-tests.
  One of the 45 is id 297 — an erroneous label (verified with pandas 3.0.3: `pd.read_csv`
  parses `tree_table.csv` cleanly to (2822, 6) and gives our exact 43.31/4.26; the label
  45.48/4.58 is not reproducible by any standard pandas operation), not a p-value issue.
- **Inexpressible — sklearn / ML models (class B): 20.** Fitted regression/classification,
  clustering, `train_test_split(random_state=…)` (needs numpy RNG).
- **Former class E (stack-overflow, NOW FIXED): 31 → 29 solved + 2 other.** The fix +
  Phases 1–3 now cover 29 (incl. 123/555 via `distinct_strings`, 453/572/574/665,
  450/451, and 468/554/760 from Phase 3). The remaining 2 are **label-discrepancy**
  (361, 662 — correct computation, wrong/degenerate label).
- **Class H — multi-number string answers: 3, NOW SOLVED** (77, 178, 219; folded into
  the 166). Assembled with `string_of_int`/`string_of_float` + `join_with`; integer-valued
  floats (`string_of_float` → `594.`) handled via `int_of_float` or a trailing-dot fixup.
- **Class C — calendar arithmetic: 0 (NOW SOLVED).** 234 (date subtraction via a
  Gregorian days-from-civil helper) and 688 (epoch→hour) are folded into the 173.
- **Class D — non-value answer: 1.** id 743 (graded value is a written-out file path;
  Hazel is pure and can't write files). `da743-income-normalize.hz` computes the
  honest part (Income min/max) and is documented/excluded.
- **Class G — degenerate grading: 1.** id 741 (graded value is the literal column name
  "ratio", not a value). `da741-balance-limit-ratio.hz` builds the honest feature
  (Balance/Limit) and is documented/excluded.
- **Parser limitation: 1.** id 618 (`#photo` column — `#` collides with comment syntax,
  so the backtick label is unparseable and the column is unreachable).
- **Label defect — 1.** id 252 ("highest-skewness country"): skewness across each
  country's year series peaks at Myanmar (Afghanistan ranks ~24/33); the label
  `Afghanistan` is the degenerate case where skewness of the single 1992 value is NaN
  and `idxmax` returns the first row. `da252-gapminder-skew.hz` emits the honest answer
  (Myanmar) and is excluded from `test.sh`, like 361/662.
  (The other former "future work" — 62, 321, 510, 589 — were solved in Phase 1.)

186 + 45 + 20 + 2 + 1 + 1 + 1 + 1 = 257. Every task is accounted for.
(45 = class-A p-value remainder; 2 = former-class-E remainder; the four trailing 1s are
D-743, G-741, parser-618, and the id-252 label defect. Class C calendar is now 0.)

## Builtins / tooling changes made for this effort

- `src/CLI/Csv.re`: **strip a leading UTF-8 BOM** from headers during ingestion, so the
  first column of BOM-prefixed CSVs (beauty, gapminder_cleaned, Current_Logan) projects.
- `src/language/builtins/BuiltinsBase.re`: `round` (added earlier in this project).
- Everything else lives in `prelude.hz` (no new builtins needed): sum/mean/pop_std/
  sample_std/median/quantile/pearson/skew/skew_pop/kurtosis/pearson_skew, round0-4,
  fmin/fmax, count_z_out/count_iqr_out, distinct/count_eq/col_where/join_with,
  num/num_commas/num_loose/num_clean (messy-cell parsers), corr_cols/corr_clean.

## Notable gotchas re-confirmed

- **Two-line comments are fatal.** `# line1\n# line2 #` — the second line's leading `#`
  closes the first comment, turning the rest into code and silently breaking the whole
  prelude chain (every downstream `mean`/`num` then prints unreduced). Keep each comment
  to ONE line with no internal `#`. This cost a debugging cycle here.
- Strings print quoted; `round2` prints fixed 2dp; huge floats carry repr noise
  (e.g. `22756785531.290001`). The `test.sh` normalizer strips trailing decimal zeros
  and is applied to BOTH sides so label precision (2/3/4 dp) matches without rounding.
