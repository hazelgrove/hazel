#!/usr/bin/env bash
# Regression test for the DA-Bench solutions.
#
# Runs every solution through run.sh (prelude + solution) and grades the output
# against the InfiAgent reference answer via check_label.py, which reads the
# external da-dev-labels.jsonl (so the gold answers are never restated here).
# Catches silent breakage from changes to prelude.hz, the CLI, or a solution.
#
# Usage:  da-bench/test.sh            # run all
#         da-bench/test.sh da0-mean-fare.hz   # run one
#         TABLES=/path da-bench/test.sh       # override data dir
#         LABELS=/path/da-dev-labels.jsonl da-bench/test.sh   # override labels file
# Exits non-zero if any case fails.
set -uo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LABELS="${LABELS:-$HOME/Projects/InfiAgent/examples/DA-Agent/data/da-dev-labels.jsonl}"

# Solution files to grade. The expected answer for each is looked up by task id
# (parsed from the daN-... filename) in the external da-dev-labels.jsonl.
CASES=(
  "da0-mean-fare.hz"
  "da5-corr-familysize-fare.hz"
  "da6-fare-by-agegroup.hz"
  "da9-mean-close.hz"
  "da14-price-range.hz"
  "da18-mar2019-iqr.hz"
  "da24-mean-age.hz"
  "da26-corr-charges-children.hz"
  "da27-charges-outliers.hz"
  "da56-max-deaths.hz"
  "da58-missing-pct.hz"
  "da174-fare-skew.hz"
  "da349-mean-age.hz"
  "da320-mean-eventmsgtype.hz"
  "da542-mean-length.hz"
  "da490-mean-engineering.hz"
  "da719-mpg-mean-median.hz"
  "da71-volume-mean-std.hz"
  "da737-income-mean-std.hz"
  "da255-gdp2007-mean-std.hz"
  "da73-corr-high-low.hz"
  "da218-corr-diffsel.hz"
  "da351-corr-age-fare.hz"
  "da409-missing-cabin.hz"
  "da217-max-diffsel-site.hz"
  "da114-max-happiness.hz"
  "da32-impscore-mean-std.hz"
  "da129-fare-std.hz"
  "da208-compound-mean-std.hz"
  "da216-absdiffsel-mean-std.hz"
  "da372-trips-mean.hz"
  "da472-value-mean.hz"
  "da506-reviews-mean.hz"
  "da578-aapl-volume-mean.hz"
  "da643-volume-stats.hz"
  "da649-x-mean-std.hz"
  "da666-mhv-mean-std.hz"
  "da683-temp-mean.hz"
  "da105-corr-income-loan.hz"
  "da209-corr-neg-pos.hz"
  "da474-corr-value-time.hz"
  "da508-corr-reviews-bubble.hz"
  "da517-corr-pclass-fare.hz"
  "da526-corr-pclass-fare.hz"
  "da650-corr-xy.hz"
  "da655-corr-xy-xz.hz"
  "da721-corr-mpg-weight.hz"
  "da739-corr-limit-balance.hz"
  "da64-wage-mean-std.hz"
  "da69-corr-expscore-wage.hz"
  "da19-mar2020-normal.hz"
  "da25-bmi-normal.hz"
  "da222-absdiffsel-skew.hz"
  "da516-fare-skew.hz"
  "da337-price-skew.hz"
  "da338-corr-sizerank-price.hz"
  "da132-fare-zout.hz"
  "da175-age-zout.hz"
  "da35-retention-zout.hz"
  "da273-meangam-zout.hz"
  "da418-volume-zout.hz"
  "da740-balance-zout.hz"
  "da352-fare-zout.hz"
  "da116-happiness-zout.hz"
  "da518-fare-zleft.hz"
  "da411-fare-iqr.hz"
  "da528-fare-iqr.hz"
  "da651-zcoord-iqr.hz"
  "da473-value-iqr.hz"
  "da247-runs-by-fa.hz"
  "da414-age-by-class.hz"
  "da527-age-by-sex-class.hz"
  "da8-fare-by-class.hz"
  "da412-familysize-by-survived.hz"
  "da108-totalincome-mean-std.hz"
  "da250-ba-obp-mean-std.hz"
  "da354-familysize-avg.hz"
  "da480-value-centered-std.hz"
  "da520-corr-familysize-survived.hz"
  "da723-power-weight.hz"
  "da425-missing-wind.hz"
  "da427-missing-minp.hz"
  "da507-star5-count.hz"
  "da586-abandoned-sum.hz"
  "da324-max-missing-col.hz"
  "da492-top-field-2010.hz"
  "da176-median-age-filtered.hz"
  "da722-max-hp.hz"
  "da117-strongest-corr.hz"
  "da733-log10-gdp.hz"
  "da726-log-horsepower.hz"
  "da272-ratio-mean-std.hz"
  "da254-gdp1982-outlier-countries.hz"
  "da587-corr-talking-wait.hz"
  "da496-stem.hz"
  "da243-batting-mean.hz"
  "da421-volume-mean.hz"
  "da543-corr-diameter-rings.hz"
  "da588-wait-zout.hz"
  "da669-medinc-iqr-stats.hz"
  "da690-windspeed-z.hz"
  "da724-accel-z-stats.hz"
  "da179-corr-age-fare-first.hz"
  "da376-trips-per-membership.hz"
  "da55-mean-cases.hz"
  "da57-corr-cases-deaths.hz"
  "da423-corr-volatility-volume.hz"
  "da310-strongest-pair.hz"
  "da271-preprocess.hz"
  "da75-daily-return.hz"
  "da180-fare-z-by-class.hz"
  "da207-missing-object-cols.hz"
  "da308-title-avg-fare.hz"
  "da309-skew-kurt.hz"
  "da619-duration-z.hz"
  "da604-swx-z.hz"
  "da220-ferret-missing.hz"
  "da732-lifeexp-missing-after.hz"
  "da59-americas-max-cases.hz"
  "da710-mean-wins.hz"
  "da716-wins-mean-std.hz"
  "da715-missing-pct.hz"
  "da28-preprocess-means.hz"
  "da111-loan-impute-std.hz"
  "da133-cabin-delete-age-median.hz"
  "da210-neg-z-stats.hz"
  "da378-trips-dist-stats.hz"
  "da495-arch-z-stats.hz"
  "da656-x-z.hz"
  "da513-corr-by-star.hz"
  "da725-displacement-mpg.hz"
  "da214-corr-sentiment-length.hz"
  "da593-waiting-ratio-skew.hz"
  "da426-max-cat-wind.hz"
  "da514-city-review-extremes.hz"
  # Large-table tasks unblocked by the tail-recursive statics/value-check fix
  # (previously class E: evaluator stack-overflow on lists of thousands of rows).
  "da551-mean-dbh.hz"
  "da446-windspeed-mean.hz"
  "da755-tmax-mean.hz"
  "da277-corr-medind-larind.hz"
  "da278-zout-agri.hz"
  "da282-corr-zout.hz"
  "da359-windspeed-skew.hz"
  "da360-corr-temp-humidity.hz"
  "da447-iqr-baro.hz"
  "da465-age-skew.hz"
  "da466-corr-count-age.hz"
  "da552-corr-ht-ba.hz"
  "da553-iqr-tph.hz"
  "da657-close-stats.hz"
  "da659-corr-high-low.hz"
  "da663-corr-high-low.hz"
  "da757-iqr-obsvalue.hz"
  "da759-tmax-median-range.hz"
  # Need the sorted-insertion distinct_strings helper (low-cardinality dedup)
  "da555-unique-species.hz"
  "da123-top-country-vacc.hz"
  # PLAN.md Phase 1: write-only solutions (no new infrastructure)
  "da62-deaths-iqr-mean.hz"
  "da574-corr-matrix.hz"
  "da453-baro-preprocess.hz"
  "da219-ferret-iqr-outliers.hz"
  "da178-titanic-preprocess.hz"
  "da321-scoremargin-iqr.hz"
  "da589-abandonment-rate.hz"
  "da572-spx-aapl-pct.hz"
  "da665-price-category.hz"
  "da77-microsoft-monthly-close.hz"
  "da510-hotel-brand-star.hz"
  # PLAN.md Phase 2: Python-dict answers via py_dict / dict_of_tuple prelude helpers
  "da450-monthly-windspeed.hz"
  "da451-missing-per-column.hz"
  # PLAN.md Phase 3: legitimately-computable edge cases (others are documented defects)
  "da468-assault-age-iqr.hz"
  "da554-median-empty.hz"
  "da760-most-missing-station.hz"
  # PLAN.md Phase 4: calendar / date arithmetic
  "da234-budget-year-duration.hz"
  "da688-time-of-day.hz"
  # PLAN.md Phase 5: hypothesis-test p-values via special-function prelude (erf/lgamma/betainc)
  "da408-fare-age-corr.hz"
  "da34-retention-importance-corr.hz"
  "da668-houseage-value-corr.hz"
  "da11-high-low-corr.hz"
  "da66-wage-exper-corr.hz"
  "da140-votes-corr.hz"
  "da452-wind-baro-dir180.hz"
  "da326-eventhour-corr.hz"
  # Phase 5 t-tests (pooled + Welch) and D'Agostino normaltest
  "da109-loan-by-education.hz"
  "da415-male-fare-survival-ttest.hz"
  "da419-weekday-weekend-ttest.hz"
  "da652-xcoord-normaltest.hz"
  "da729-gdp-normaltest.hz"
  # Phase 5 ANOVA (F-test via betainc) and chi-square (incomplete gamma)
  "da428-storm-damage-anova.hz"
  "da522-title-pclass-chi2.hz"
  # Phase 5 Kolmogorov-Smirnov (one- and two-sample) and Mann-Whitney U
  "da33-mz-ks-normality.hz"
  "da658-volume-ks-normality.hz"
  "da410-age-ks-2samp.hz"
  "da177-age-class-mannwhitney.hz"
  # Phase 5 Shapiro-Wilk (Royston AS R94 W + n>=12 p-transform, probit via Acklam)
  "da10-tradedqty-shapiro.hz"
  "da39-impscore-shapiro.hz"
  "da72-close-shapiro.hz"
  "da130-age-shapiro.hz"
  "da136-fare-shapiro-byclass.hz"
  "da139-perdem-shapiro.hz"
  "da244-homeruns-shapiro.hz"
  "da268-meanpot-shapiro.hz"
  "da304-fare-shapiro.hz"
  "da350-fare-shapiro.hz"
  "da375-trips-distribution.hz"
  "da449-windspeed-shapiro.hz"
  "da602-rhoold-shapiro.hz"
  "da644-close-shapiro.hz"
  "da647-pricerange-shapiro.hz"
  "da667-medinc-shapiro.hz"
  "da684-humidity-shapiro.hz"
  "da736-popgdp-shapiro.hz"
  "da738-age-shapiro.hz"
  # Phase 6A — write-only sweep (existing prelude: pearson_p / anova_p / corr; no new infra)
  "da142-votes-ppd-corr.hz"
  "da269-totusjh-totusjz-corr.hz"
  "da413-pclass-fare-cherbourg.hz"
  "da529-sibsp-parch-corr.hz"
  "da530-age-fare-by-sex.hz"
  "da575-techavg-spx-corr.hz"
  "da685-pressure-wind-corr.hz"
  "da730-pop-gdp-corr.hz"
  "da144-pervote-meanstd.hz"
  "da249-doubles-salary-corr.hz"
  "da429-stormcat-damage-corr.hz"
  "da550-abalone-dist.hz"
  "da673-mhv-roomsperperson.hz"
  "da124-vaccine-anova.hz"
  "da734-continent-lifeexp-gdp.hz"
  "da756-tmax-obs-corr.hz"
  # Phase 6B — OLS regression: simple R^2=r^2 (118), and 2-predictor OLS via mean-centered normal
  # equations (new `ols2`: coefficients, slope p-values via t_sf2, and R^2 = statsmodels OLS)
  "da118-gdp-lifeexp-r2.hz"
  "da355-fare-age-pclass-ols.hz"
  "da125-vaccine-multiols.hz"
  # Phase 6D (RNG-free): feature engineering / duplicate count (no model, no split needed)
  "da275-flux-dedup-feature.hz"
  "da590-predict-agents.hz"
  # Phase 6C: numpy-compatible MT19937 + train_test_split (random_state=42) + OLS on the split.
  # Pure-Hazel RNG: list-MT (O(n) twist) + recursive-ADT tree Fisher-Yates; matches np.RandomState(42).
  "da727-mpg-split-mse.hz"
  "da23-unemployment-mse.hz"
  "da671-medhouseval-mse.hz"
  "da30-insurance-rmse.hz"
  "da70-wage-rmse.hz"
  "da549-abalone-volume-rmse.hz"
  "da363-sun-mse.hz"
  # Phase 6D: logistic regression via IRLS (prelude `logreg`); impute missing Age with the mean
  "da521-survival-logreg.hz"
  "da224-diffsel-logreg.hz"
  # Phase 6D: linear-regression classifier (one-hot Sex+Embarked, threshold 0.5 via olsk)
  "da7-survival-linreg-onehot.hz"
)

if [ ! -f "$LABELS" ]; then
  echo "labels file not found: $LABELS" >&2
  echo "set LABELS=/path/to/da-dev-labels.jsonl (from the InfiAgent clone)" >&2
  exit 2
fi

# Run a solution and return its last output line, raw: check_label.py handles
# float precision itself, and normalizing here would corrupt string answers.
run_one() {
  "$DIR/run.sh" "$1" 2>&1 | grep -vE 'joo_global_object|deprecat|Terminated' | tail -1
}

only="${1:-}"
pass=0; fail=0
for c in "${CASES[@]}"; do
  file="${c%%|*}"                       # tolerate legacy "file|expected" entries
  [ -n "$only" ] && [ "$only" != "$file" ] && continue
  id="${file#da}"; id="${id%%-*}"       # da7-survival-...hz -> 7
  got="$(run_one "$file")"
  if python3 "$DIR/check_label.py" "$id" "$got" "$LABELS"; then
    printf 'PASS  %-34s %s\n' "$file" "$got"; pass=$((pass+1))
  else
    printf 'FAIL  %-34s\n        got [%s]\n' "$file" "$got"; fail=$((fail+1))
  fi
done
echo "----"
echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
