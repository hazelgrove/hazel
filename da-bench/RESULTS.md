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

## Tally (257 dev tasks) — 242 solved, 15 accounted for

- **Solved & verified — 242** (in `test.sh`, every output matches the InfiAgent label):
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
  t-tests (109, 415 pooled; 419 Welch), D'Agostino normaltest (652, 729), one-way ANOVA
  (428, 124-class), chi-square (522), Kolmogorov-Smirnov (33, 410, 658), Mann-Whitney (177),
  and the full Shapiro-Wilk bucket (19 tasks: 10, 39, 72, 130, 136, 139, 244, 268, 304, 350,
  375, 449, 602, 644, 647, 667, 684, 736, 738) via new `probit` (Acklam) + `shapiro_w`/
  `shapiro_p` (Royston AS R94), verified exact vs scipy.stats.shapiro.
  Plus PLAN.md **Phase 6A** (16 tasks) — write-only with the *existing* prelude (no new infra):
  Pearson-r p-value / relationship / significance (142, 269, 413, 429, 529, 530, 575, 685, 730,
  249, 756), per-continent correlation (734), one-way ANOVA per-country (124, vaccine groups),
  and descriptive/preprocessing answers (144 mean/std of per-vote, 550 abalone distribution-type
  via skew/kurtosis, 673 RoomsPerPerson correlation + mean). All cross-checked against real
  scipy/pandas before trusting the label. (These were always expressible with the Phase-5
  `pearson_p`/`anova_p` library — they were simply never written; PLAN.md mis-scoped "Phase 6"
  as ~20 ML+RNG when ~16 of the 38 remaining needed no new code at all.)
  Plus PLAN.md **Phase 6B** (3 tasks) — OLS regression: 118 (simple regression R^2 = r^2, "poor fit"),
  and via a new `ols2` prelude helper (two-predictor OLS through the mean-centered normal equations —
  coefficients, slope p-values via `t_sf2`, and R^2, all matching statsmodels OLS exactly): 355
  (Fare ~ Age + Pclass coefficients + significance) and 125 (vaccine multiple regression R^2 + both
  predictors significant). id 590 SOLVED after a `String.trim` on CSV headers in `Csv.re` made its
  trailing-space column "avg. num. agents staffed " reachable (the target is constant 4 -> predict 4);
  id 432 moved to 6C (needs a train/test split).
  Plus PLAN.md **Phase 6C** (train_test_split + regression): a **pure-Hazel, numpy-exact MT19937 +
  Fisher-Yates `train_test_split`** was built in `prelude.hz` and verified bit-identical to
  `np.random.RandomState(42).permutation(n)` (the MT state is a list so the twist is an O(n) XOR pass;
  a recursive-ADT perfect-tree "functional array" does the O(log n) shuffle swaps; a `Float` tree
  `ArrF` gathers columns by permuted index). With it, OLS-on-the-split solves **727** (mpg, test
  MSE=17.66), **23** (employment, simple regression MSE=11439.6 via `ols1`), **30** (insurance,
  RMSE=11464.74 via `ols2`), **671** (5-feature MSE=0.653 via general `olsk`), **70** (12-feature
  RMSE=3.63), **549** (abalone correlation + original vs original+volume RMSE — all three). Plus
  Phase **6D RNG-free 275** (duplicate count + engineered-feature mean; the RF step isn't graded).
- **scipy p-value / hypothesis tests (class A): 58 → 20 (Phase 5 DONE for all named tests).**
  The special-function prelude now does Pearson-r p, pooled/Welch t-tests, D'Agostino
  normaltest, ANOVA (F via betainc), chi-square (gammainc), Kolmogorov-Smirnov, Mann-Whitney,
  and Shapiro-Wilk (probit + Royston) — all exact vs scipy (38 solved). The remaining 20 are
  mostly regression-based significance (need a fitted model → Phase 6) plus two erroneous-label
  discrepancies (297 t-test means, 298 Shapiro 'yes' — both the tree_table.csv whose label is
  unreproducible).
  Note id 297 — an erroneous label (verified with pandas 3.0.3: `pd.read_csv`
  parses `tree_table.csv` cleanly to (2822, 6) and gives our exact 43.31/4.26; the label
  45.48/4.58 is not reproducible by any standard pandas operation), not a p-value issue.
- **ML / regression (Phase 6B/6C/6D): 20 → 15 solved, 5 documented.** SOLVED: full-data/coefficient
  OLS 118, 125, 355; RNG-free feature-eng 275; the seeded-split regressions 23, 30, 70, 363, 549,
  671, 727 (the train/test split uses a **native `np_permutation` builtin** — numpy-exact MT19937 +
  Fisher-Yates in OCaml, `src/language/builtins/BuiltinsBase.re` — plus `ols1`/`ols2`/`olsk`; the
  pure-Hazel MT19937 in `prelude.hz` is kept as a verified reference); **521** logistic regression
  via a new IRLS `logreg` helper (the gap was preprocessing: the reference *imputes* missing Age with
  the mean -> 0.78; dropping those rows gives 0.76 — unregularized IRLS converges to the same optimum
  sklearn reaches); **224** logistic on `positive_diffsel` (the class is defined *as* `> mean`, so the
  feature perfectly separates the data; the label 0.98 comes from `liblinear`'s intercept-penalized L2
  shrinking the otherwise-infinite coefficients — matched with a regularized `logreg_l2(…, lam=1/C=1)`
  IRLS helper; unregularized / lbfgs give 1.0); and **7** linreg-as-classifier over one-hot
  Sex/Embarked (drop the 1-row redundant `Sex='0'` dummy for full rank, `olsk`, 0.5 threshold -> 0.78).
  363 (n=16,684) needs the raised node heap (`--max-old-space-size` in `run.sh`).
  DOCUMENTED (not matched):
  - **137** — logistic with `class_weight='balanced'` on the `IsAlone` feature. The label 0.61 is not
    reproduced by sklearn either: unweighted = 0.59, balanced = 0.64 (neither is 0.61). A benchmark quirk.
  - **674** (decision-tree regression, pearson/MAE) — a from-scratch CART (best-SSE split, midpoint
    thresholds, `max_depth=5`) reproduces sklearn's root and ~96% of the tree, but diverges at a
    5-sample node where **two features tie on the split proxy bit-for-bit** (Latitude and Longitude
    both = 104.4915, each isolating the lone `3.629` sample). sklearn breaks that tie with its
    *internal* tree-builder RNG (`our_rand_r` feature-shuffle, seeded from `random_state` — a different
    RNG from numpy's MT19937), picking Longitude; a first-feature tie-break gives 0.6553, last-feature
    0.6324, neither the label 0.6419. Matchable only by porting sklearn's internal C RNG + exact
    feature-visitation order (brittle across versions); not done.
  - **424** (random forest feature-importance) — strictly harder than 674: needs the seeded ensemble
    (bootstrap resampling + per-split feature subsampling × 100 trees) *and* each tree's internal RNG.
  - **523** (KNN Age-imputation): child_count=72 not reproducible even in sklearn (got 91/69), so the
    exact tie-breaking/scaling is unclear; not matched.
  - **432** — its 80/20 split metric (MSE 263.19) is not reproducible by any seed/split tried (the task
    states no `random_state`).
  (590 is now SOLVED — a `String.trim` on CSV headers in `Csv.re` makes its trailing-space column
  "avg. num. agents staffed " reachable; the target is constant 4, so the predicted value is 4.)
- **Phase 6A discrepancies (documented, excluded from `test.sh`): 2.**
  - **300** (nsnps~nsamplecov on `…tree_table.csv`): our Pearson r is **0.53** (pandas `df.corr`
    agrees, n=2657 pairwise-dropna), but the label is **0.54**, unreproducible by any standard
    Pearson preprocessing. Same `tree_table.csv` file whose labels are wrong for **297/298**.
    The "correlated" verdict (p<0.05) matches; only the coefficient is off by 0.01.
    `da300-…hz` emits the honest 0.53.
  - **431** (max_storm_cat~duration, high-damage split, `cost_data_with_errors.csv`): the
    duration must be parsed from en-dash + non-breaking-space month ranges ("July 30 – August 1"),
    impractical in Hazel's ASCII string ops. A pandas calendar-correct duration gives high-damage
    r=**0.57** vs label **0.56** anyway (relationship_type "linear" and p=0.0000 *do* match) — a
    duration-parse convention difference. Not implemented.
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

242 + 5 + 10 = 257. Every task is accounted for.
(242 solved incl. Phases 6A/6B/6C/6D-275 + the native-RNG splits 23/30/70/363/549/671/727 + 590 + the
521/224 logistic regressions + the 7 one-hot linreg classifier; 5 ML/regression documented-not-matched
— 137 (label 0.61 not reproduced by sklearn either), 424 (random forest), 674 (decision tree), 523
(KNN imputation), 432 (unreproducible split, no stated random_state); 10 = documented defects/
discrepancies + the parser blocker: 252, 297, 298, 300, 361, 431, 618, 662, 741, 743.)

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
