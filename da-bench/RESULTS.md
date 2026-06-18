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

## Tally (257 dev tasks) — 155 solved, 102 accounted for

- **Solved & verified — 155** (in `test.sh`, every output matches the InfiAgent label):
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
- **Inexpressible — scipy p-value / hypothesis tests (class A): 58.** Shapiro, KS,
  normaltest, t-test, ANOVA, chi-square, Mann-Whitney, etc. A coefficient is fine; the
  test statistic's p-value is not (no distribution CDFs / special functions).
- **Inexpressible — sklearn / ML models (class B): 20.** Fitted regression/classification,
  clustering, `train_test_split(random_state=…)` (needs numpy RNG).
- **Former class E (stack-overflow, NOW FIXED): 31 → 20 solved + 11 other.** The 20
  listed above now pass (incl. 123 and 555 via `distinct_strings`). The remaining 11:
  **2 label-discrepancy** (361, 662 — correct computation, wrong/degenerate label),
  **2 Python-dict output** (450, 451), **2 degenerate spec** (554 nan, 760 all-zero),
  **1 ambiguous** (468), and **4 multi-step left as future work** (453, 572, 574, 665).
- **Class H — number→string formatting gap: 3.** ids 77, 178, 219 pack several computed
  numbers into ONE answer field (e.g. `"314, 577"`, `"1, 2018, 88.32"`); there is no
  number→string builtin to assemble that string, so the graded value can't be produced.
- **Class C — calendar arithmetic: 2.** id 234 (days between two calendar dates),
  id 688 (epoch→hour-of-day bucketing). Substring/`HH:MM:SS`→sec IS fine; calendar math isn't.
- **Class D — non-value answer: 1.** id 743 (graded value is a written-out file path).
- **Class G — degenerate grading: 1.** id 741 (graded value is the literal column name "ratio").
- **Parser limitation: 1.** id 618 (`#photo` column — `#` collides with comment syntax,
  so the backtick label is unparseable and the column is unreachable).
- **Expressible but left as future work — 5.** ids 62, 252, 321, 510, 589: multi-step
  per-group IQR (62), ambiguous single-value skewness (252), `SCOREMARGIN` sign/`TIE`
  cleaning (321), brand-filter argmax (510), and a timestamp row that isn't present as
  written (589). No language barrier — just tedious/under-specified; not attempted.

155 + 58 + 20 + 11 + 3 + 2 + 1 + 1 + 1 + 5 = 257. Every task is accounted for.
(The 11 is the former-class-E remainder; its 20 solved tasks are inside the 155.)

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
