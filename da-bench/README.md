# DA-Bench in Hazel

An experiment measuring how **expressively** the Hazel language can solve real
data-analysis problems. We hand-write `.hz` programs that answer InfiAgent
**DA-Bench** dev-set questions over Hazel's labeled-tuple "tables", and check the
printed result against the benchmark's reference label.

This is **not** an agent evaluation. We are measuring the *language*: can Hazel
express the computation and produce the correct answer? Solutions are plaintext
`.hz` files; data is loaded at edit-time via the `^^csv("file.csv")` hook (no
runtime side effects — the language stays pure).

**Status: 155 / 257 dev tasks solved & verified.** The authoritative ledger of
what passes / what's left and why is [`RESULTS.md`](./RESULTS.md); the
authoritative list of passing cases is the `CASES` array in `test.sh` (every
entry is checked against the InfiAgent label).

---

## Layout & sources

- Questions: `~/Projects/InfiAgent/examples/DA-Agent/data/da-dev-questions.jsonl`
  (`{id, question, constraints, format, file_name, level}`)
- Labels:    `~/Projects/InfiAgent/examples/DA-Agent/data/da-dev-labels.jsonl`
  (`{id, common_answers: [[key, value], ...]}`)
- CSVs:      `~/Projects/InfiAgent/examples/DA-Agent/data/da-dev-tables/*.csv`
- Solutions live here as `daN-<slug>.hz`; shared helpers in `prelude.hz`.

## Running a solution

`run.sh` prepends the shared prelude and points at the data dir:

```bash
da-bench/run.sh da0-mean-fare.hz
# TABLES=/other/tables da-bench/run.sh daN-foo.hz   # override data dir
```

Under the hood it concatenates `prelude.hz` + your solution into a temp file and
runs `./hazel run <tmp> --data-dir <tables> --yes`. To call `hazel` directly:

```bash
cat da-bench/prelude.hz da-bench/daN-foo.hz > /tmp/run.hz
./hazel run /tmp/run.hz \
  --data-dir ~/Projects/InfiAgent/examples/DA-Agent/data/da-dev-tables --yes
```

- `--data-dir` is where a relative `^^csv("file.csv")` path resolves.
- `--yes` skips the file-access consent prompt (drop it for an interactive
  allow / substitute-path / deny prompt).
- Also: `./hazel analyze … --yes` (typecheck only), `./hazel expand … -o out.hz`
  (materialize a self-contained `.hz` with the table inlined as text).

## Regression testing

`test.sh` runs every solution through `run.sh` and compares its output to the
reference answer (decimals normalized to strip trailing zeros, so `34.650000`
matches `34.65` and precision like `0.7366` is preserved). A silently *stuck*
result (an unreduced expression) won't match either, so it shows up as a FAIL.

```bash
da-bench/test.sh                 # all cases; exits non-zero if any fail
da-bench/test.sh da0-mean-fare.hz  # one case
```

**When you add a solution, add a `"<file>|<expected>"` line to `CASES` in
`test.sh`** (expected = the label answer; floats keep their stated precision,
ints/strings verbatim). Run the full suite after touching `prelude.hz` or the
CLI. Note: large-table cases take 30–100 s each (see Performance), so a full run
is minutes.

## How CSV data gets in: `^^csv("file.csv")`

Write `let data = ^^csv("file.csv") in …`. The CLI replaces the reference with
the table **before** evaluation — so the language itself performs no I/O. Two
implementations (see `src/CLI/Csv.re`):

- `run`/`analyze`: build the table directly as **AST** (fast — only the small
  program skeleton is parsed; the table is never re-parsed).
- `expand`: emit a `^^table([...])` **text** literal (portable, but slow to
  re-parse — don't `expand` then `run` a big table; just `run`).

Every cell is a **String** (like `grade.hz`). Parse what you need yourself.

This edit-time-injection pattern is the key to keeping the language pure while
still working on real data: the *tooling* reads the file and splices a literal;
the program is a pure function of that literal. The same pattern is the planned
route for randomness (see "RNG" under Limitations).

## The stats prelude

There is **no import mechanism** for the `run` path, so shared helpers live in
`prelude.hz` (a chain of `let … in` bindings ending with a dangling `in`).
`run.sh` concatenates it in front of the solution, so the solution expression
becomes the body — **don't redefine these in solutions, just use them:**

- `sum`, `mean`
- `pop_std` (ddof=0), `sample_std` (ddof=1 — pandas `.std()` default)
- `median` (+ `cmp`), `quantile(sorted_xs, q)` (linear interpolation = numpy default)
- `pearson(xs, ys)`; `corr_cols(a, b)` / `corr_clean(a, b)` — Pearson r over two String
  columns, dropping rows where either cell is blank (pairwise dropna). `corr_clean`
  also strips brackets/commas. Use these for almost every correlation task.
- `skew(xs)` — pandas adjusted Fisher-Pearson; `skew_pop(xs)` — scipy.stats.skew (biased);
  `kurtosis(xs)` — scipy Fisher excess (biased); `pearson_skew(xs)` — 3·(mean−median)/std
- `round0`–`round4`; `fmin`/`fmax`
- `count_z_out(xs, thresh)` — count of |z|>thresh using population std (scipy zscore);
  `count_iqr_out(xs)` — count outside Q1−1.5·IQR … Q3+1.5·IQR
- `distinct(xs)` — first-seen-order dedup, **O(n²)**, only for small columns;
  `distinct_strings(xs)` — sorted-insertion dedup over `string_compare`, accumulator
  stays at distinct-count size, so it's practical for large low-cardinality columns
- `count_eq(xs, v)`, `col_where(keys, vals, k)` (values where key==k — for group-by),
  `join_with(sep, xs)`
- `num(col)` — trim + drop blanks + `float_of_string`. Variants: `num_commas`
  (tolerates `5,350,380` thousands separators), `num_loose` (drops non-numeric cells
  via `string_match`, e.g. a `null` sentinel or a stray header leak), `num_clean`
  (leading number before a `[` bracket, e.g. `298[110-510]`).

A typical solution is just:

```
# DA-Bench id N: <task>, expect <answer> #
let data = ^^csv("file.csv") in
round2(mean(num(data.`Column`)))
```

Add task-specific `let`s as needed; they can reference prelude helpers. Unused
helpers produce only harmless warnings. To add a broadly-useful helper, put it in
`prelude.hz` (and re-run the full `test.sh`, since every solution shares it).

## Reading columns

- **Project a whole column** with `.`: `` data.`Fare` `` is the list of all Fare
  cells. Don't `map` just to extract a column.
- **Backtick every header.** CSV headers are usually capitalized or spaced; unquoted
  Hazel identifiers must start lowercase, so always write `` data.`Header` ``.
- Cells are strings, often **quoted and space-padded** (e.g. `"        578.55"`).
  `float_of_string` rejects whitespace → `string_trim` first. `num` does trim +
  drop-blank + parse.
- **Missing values** are empty strings `""` (filter them before parsing). Some files
  use a literal `null` or `?` sentinel instead — use `num_loose` to drop those.
- The unnamed pandas index column (empty header) becomes `` `col0` ``.
- For row-aligned work across columns, `zip(colA, colB)` then filter the *pair* on
  both cells being present (filtering one column alone misaligns the pair).

## Language gotchas (the tylr surface syntax — same as table.hz/grade.hz)

- **Comments are `#...#`** — `#` is BOTH the open and close delimiter, so a comment
  must contain no internal `#`. A stray `#` closes the comment early and turns the
  rest into code.
- **NEVER write a two-line comment.** `# line one\n# line two #` parses as comment
  `# line one #` (closed by line two's leading `#`), then `line two` as CODE. In
  `prelude.hz` this silently breaks the whole `let … in` chain — every downstream
  helper then prints *unreduced* with no error. Keep each comment on ONE line.
- **Recursion is built into `let`** — `let f = fun x -> … f(…) …` may call itself, no
  special keyword. `^^fold` is just a *display* projector, not required for recursion.
- **Matching:** `case e | pat => … | pat => … end` (terminated by `end`); list
  patterns `(y :: rest)` / `| []`; cons construction `x :: xs`.
- **`string_compare(a, b)` returns `Lt`/`Eq`/`Gt`** (use as the `sort` comparator).
  There is **no** string `<`/`>` operator — only `==` for equality.
- **No trailing commas.** `f(a, b, c,)` parses as a tuple with an extra hole → wrong
  arity → silently *stuck* (prints unreduced, not an error).
- **No `not`** — use prefix `!` or `!=`.
- **Negative float literals:** write `0. -. 2.5`, not `-. 2.5`.
- Float ops are dotted: `+. -. *. /. **. <. >. <=. >=. ==.`. Int division `/`, `int_mod(n,m)`.
- `|>` pipe and `_` placeholder work; lambdas destructure tuples (`fun (x, y) -> …`).
- `sort(cmp, xs)` takes the comparator first.
- Results print via `%f` (6 decimals): `34.65` prints as `34.650000`.
- `fst`/`snd` exist. An argmax `fold` seeded from `head(xs)`/`tail(xs)` can leave the
  result indeterminate; prefer mapping to `(key, value)` pairs and folding with a
  sentinel init (`("", 0. -. 1.0e6)`).

## Useful builtins

`map filter fold_left length zip sort nth take drop reverse`,
`float_of_string int_of_string string_trim string_split string_match string_compare`,
`sqrt abs_float floor ceil round exp log`, `float_of_int int_of_float`,
`string_of_int string_of_float` (number→string; `string_of_float` is shortest
round-trip — `7.17`, `0.25` — but integer-valued floats print OCaml-style as `594.`
/ `7.`, not Python's `594.0`),
`group_by_label to_lvs omit_labels pivot_table`. There is **no** built-in
`sum`/`mean`/`median`/`std` — compose them (see prelude).

## Matching the reference answer (spec fidelity, not language limits)

The language expresses all of these; you just have to pick pandas' convention:

- **std defaults to sample (`ddof=1`)** in pandas `.std()` → use `sample_std` unless
  the question says "population". For large n the rounded answer can differ by 0.01.
- **Skewness convention matters:** some labels use the *moment* skew (`skew_pop`/`skew`)
  even when the prompt says "Pearson's coefficient" (e.g. id 359's 0.83 is the moment
  value, not 3·(mean−median)/std = 0.66). Try both if one misses.
- **Binning boundaries / rounding:** mind half-open vs closed intervals; `round2` for
  "2 decimal places", `round4` where asked.

---

# Limitations: what actually blocks us

Every unsolved task is classified in `RESULTS.md`. The important framing: **almost
none of these are limitations of Hazel as a language** — they're missing libraries
or simply unwritten code that Hazel could express. Only one is a true language
blocker, and a handful are defects in the benchmark itself, not in Hazel.

## Hard blocker — a genuine language limitation

**`#` in a column name (parser).** Hazel comments are delimited by `#` on both
ends, and `#` is special *even inside backtick-quoted labels*. A column literally
named `#photo` (traj-Osak dataset) cannot be projected: `` data.`#photo` `` fails
to parse because the `#` opens a comment. There is no escape sequence. This is a
real surface-syntax limitation of the tylr parser — a solution cannot work around
it; it would take a *language* change (teach the lexer to ignore `#` inside
backticks). **Affects id 618.** This is the only true hard blocker.

## Addressable — missing library or effort, NOT a language limit

Unsolved today, but Hazel *can* express them. These are work items, not blockers.

### 1. Statistical distribution functions / p-values — ~58 tasks (the biggest bucket)

Hypothesis-test tasks (Shapiro-Wilk, Kolmogorov-Smirnov, t-test, ANOVA,
chi-square, Mann-Whitney) want a **p-value** or an accept/reject decision. We
already compute the *test statistic* (Pearson r, skewness, the t-value); what's
missing is the **distribution CDF / special functions** — error function,
incomplete gamma, incomplete beta — that turn a statistic into a p-value. Those
are ordinary pure numeric routines: they can be written in Hazel (a prelude of
`erf`, `gammainc`, `betainc`, …) or added as builtins. **Effort, not impossibility.**

### 2. ML models + reproducible RNG — ~20 tasks

Two parts:

- **The model fit** (linear/logistic regression, etc.) is often expressible via
  closed-form normal equations — just unwritten.
- **Reproducible randomness** (`train_test_split(random_state=42)`, sampling,
  shuffling). Hazel is pure, so there's no ambient RNG — but that's not a dead
  end. **Planned approach:** inject the **seed at edit-time from the CLI**, exactly
  like `^^csv` injects data — e.g. a `^^seed(42)` hook (or a `--seed N` flag) that
  splices the literal seed into the program. Then provide **pure** functions that
  derive a deterministic pseudo-random stream from that seed (a PRNG written in
  Hazel: `seed -> (value, next_seed)` split/advance functions). Because the seed
  is a compile-time literal and the generator is a pure function, the language
  stays effect-free while randomness becomes reproducible across runs. The
  *remaining* hard part is **bit-exact compatibility**: matching numpy's specific
  generator (Mersenne Twister / PCG64) and `train_test_split`'s exact shuffle order
  so our split equals theirs. Purity is solved by the design; replicating numpy's
  exact stream is the work.

### 3. Multi-number string answers — ~6 tasks ("the formatting issue")

Some answers aren't a single number but a **string assembled from several numbers**
or a **Python data-structure repr**:

- id 178's answer is the literal string `"314, 577"`; ids 77 / 219 pack several
  values into one field like `"1, 2018, 88.32"`.
- id 450's answer is a Python dict `{'month_1': 7.17, 'month_2': 6.53, …}`;
  id 451 is `{'WINDSPEED': 594, 'AT': 590, …}`.

These are **expressible — just unwritten.** Hazel has `string_of_int` /
`string_of_float` and `++` / `join_with`, so the comma-list answers (77/178/219)
can be assembled directly (compute the numbers → stringify → join with `", "`).
The Python-dict answers (450/451) can be built the same way, with the extra
fiddle of emitting Python's exact `{'k': v}` text (braces, single-quotes, spacing)
and float repr (`string_of_float` prints integer-valued floats as `594.`, where
Python wants `594.0` / `594`) to satisfy the string-equality grader. Pure
serialization work, **no language limit** — just not yet done.

## Not Hazel's problem — benchmark artifacts (~6 tasks, "the degenerate issue")

Here the **reference label itself** is a non-answer, so even perfect expressiveness
has nothing meaningful to match — you'd only be reverse-engineering a trivial or
buggy ground truth:

- **Empty-result `nan` (id 554):** "median HT_M where CON=1 and PLTID=5" — the
  filter matches **zero rows**, so pandas returns `NaN` and the label is literally
  `nan`. "Solving" it means detecting the empty filter and printing the word "nan".
- **All-zero / arbitrary tie (id 760):** "which station has the most missing
  obs_value?" — **no** station has any missing values, so every count is 0; the
  label `("AGE00135039", 0)` is just whichever station pandas' `idxmax` returned
  first on an all-zero series. Matching it = reproducing an arbitrary tie-break.
- **Answer is a column name (id 741):** the graded value is the literal string
  `"ratio"` (a column name), not a computed result.
- **Ambiguous spec (id 468):** "'Assault' category" doesn't map cleanly to a column.
- **Wrong / quirky label — ids 361 and 662** (see the per-task notes at the top of
  `da361-zout-windspeed.hz` and `da662-pricechange-stats.hz`). In both, our
  computation is correct and the *label* is the outlier:
  - **361** (wind-speed Z-outliers): we report "how many outliers are there" =
    **97** (pandas and numpy agree). The prompt also says "remove the outliers and
    create a new dataframe", and the reference reported the outlier count *of that
    cleaned dataframe* — which is **0** by construction. So the label measures
    outliers-remaining-after-removal; emitting 0 would be tautological.
  - **662** (median of Close−Open): the prompt says to use Python's `statistics`
    module. With n=2175 (odd) the median is the single middle value =
    **1.30099 → 1.30** (`statistics.median` and `numpy` agree); the std **284.61**
    matches. The label's **1.31** comes from a quirk we couldn't reproduce (likely
    different `null`-handling or a different median index). Hazel is *more* correct.

These are listed for completeness in `RESULTS.md` but not pursued: fixing them would
mean emulating a defect, not improving the language.

## Already addressed in this iteration

- **Evaluator stack overflow — the biggest *fake* limit, now fixed.** Tables beyond
  ~2000 rows (sort/median) / ~9796 rows (mean) used to `Stack overflow`. Root cause:
  non-tail-recursive, per-row traversals in statics elaboration, the value checker,
  and the pretty-printer — plus an O(n²) `xs @ [x]` accumulator in
  `StaticsBase.map_m`/`map_m2`. Made tail-recursive (new `src/util/ListUtil.re`
  helpers + `StaticsBase` / `Statics` / `ValueChecker` / `ExpToSegment`).
  sort/median/quantile/correlation now run on the full tables (tree 9796,
  weather_train 16683). **Unblocked 18 tasks.** This was always a *runtime* limit,
  never an expressiveness one.
- **Distinct over large columns.** The O(n²) list-membership `distinct` was too slow
  once it stopped overflowing. Added `distinct_strings` (recursive sorted-insertion
  dedup over `string_compare`; accumulator only grows to the distinct count).
  **Unblocked 123, 555.** (~90 s on 9796 rows — see Performance.)
- **UTF-8 BOM on the first header** — stripped at ingestion in `src/CLI/Csv.re`, so
  the first column of BOM-prefixed files (beauty, gapminder_cleaned, …) projects.

---

## Performance

- The editor parser is slow on large literals; the AST-splice `run`/`analyze` path
  avoids it (parses only the skeleton). Never hand-inline thousands of rows or
  `expand`-then-`run` — reference the CSV with `^^csv(...)` and `run`.
- The evaluator is a tree-walking interpreter, so per-element work has high constant
  overhead. With the tail-recursion fix the **stack** ceiling is gone, but large-table
  runs are still slow: a full sort/correlation over ~10k rows takes ~30–100 s, and the
  `distinct_strings` tasks (123, 555) are near the top of that range. Nothing is
  blocked by this — but budget minutes, not seconds, for the big-table cases and the
  full `test.sh` sweep.
