# DA-Bench in Hazel — implementation tips

Notes for writing `.hz` solutions to InfiAgent DA-Bench data-analysis questions.
Goal: measure how expressively Hazel's labeled-tuple "tables" can solve these,
checking printed results against the benchmark labels.

## Layout & sources

- Questions: `~/Projects/InfiAgent/examples/DA-Agent/data/da-dev-questions.jsonl`
  (`{id, question, constraints, format, file_name, level}`)
- Labels:    `~/Projects/InfiAgent/examples/DA-Agent/data/da-dev-labels.jsonl`
  (`{id, common_answers: [[key, value], ...]}`)
- CSVs:      `~/Projects/InfiAgent/examples/DA-Agent/data/da-dev-tables/*.csv`
- Solutions live here as `daN-<slug>.hz`.

## Running a solution

Use the wrapper, which prepends the shared prelude (see below) and points at the
data dir:

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
- `--yes` skips the file-access consent prompt (drop it to get an interactive
  allow / substitute-path / deny prompt).
- Also: `./hazel analyze … --yes` (typecheck only), `./hazel expand … -o out.hz`
  (materialize a self-contained `.hz` with the table inlined as text).

## Regression testing

`da-bench/test.sh` runs every solution through `run.sh` and compares its output
to the InfiAgent reference answer (decimals normalized to 2 dp, so `34.650000`
matches `34.65`). Run it after touching `prelude.hz`, the CLI, or any solution:

```bash
da-bench/test.sh                 # all cases; exits non-zero if any fail
da-bench/test.sh da0-mean-fare.hz  # one case
```

A silently *stuck* result (unreduced expression) won't match its expected value,
so it shows up as a FAIL too. **When you add a solution, add a `"<file>|<expected>"`
line to the `CASES` array in `test.sh`** (expected = the label answer, floats to
2 dp; ints/strings verbatim).

## How CSV data gets in: `^^csv("file.csv")`

Write `let data = ^^csv("file.csv") in …`. The CLI replaces it with the table
**before** evaluation. Two implementations (see `src/CLI/Csv.re`):

- `run`/`analyze`: build the table directly as **AST** (fast — only the small
  program skeleton is parsed; the table is never re-parsed).
- `expand`: emit a `^^table([...])` **text** literal (portable, but slow to
  re-parse — don't `expand` then `run` a big table; just `run`).

Every cell is a **String** (like `grade.hz`). Parse what you need yourself.

## The stats prelude

There is **no import mechanism** for the `run` path, so shared helpers live in
`da-bench/prelude.hz` (a chain of `let … in` bindings ending with a dangling
`in`). `run.sh` concatenates it in front of the solution, so the solution
expression becomes the body — **do not redefine these in solutions**, just use
them:

- `sum`, `mean`
- `pop_std` (ddof=0), `sample_std` (ddof=1 — pandas `.std()` default)
- `median` (+ `cmp`), `quantile(sorted_xs, q)` (linear interpolation = numpy default)
- `pearson(xs, ys)`; `corr_cols(a, b)` / `corr_clean(a, b)` — Pearson r over two String
  columns, dropping rows where either cell is blank (pairwise dropna). `corr_clean`
  also strips brackets/commas. Use these for almost every correlation task.
- `skew(xs)` — pandas adjusted Fisher-Pearson; `skew_pop(xs)` — scipy.stats.skew (biased);
  `kurtosis(xs)` — scipy Fisher excess (biased); `pearson_skew(xs)` — 3·(mean−median)/std
- `round0`/`round1`/`round2`/`round3`/`round4`; `fmin`/`fmax`
- `count_z_out(xs, thresh)` — count of |z|>thresh using population std (scipy zscore);
  `count_iqr_out(xs)` — count outside Q1−1.5·IQR … Q3+1.5·IQR
- `distinct(xs)`, `count_eq(xs, v)`, `col_where(keys, vals, k)` (values where key==k —
  for group-by), `join_with(sep, xs)` (comma-list answers)
- `num(col)` — trim + drop blanks + `float_of_string`. Variants:
  `num_commas` (tolerates `5,350,380` thousands separators),
  `num_loose` (drops non-numeric cells via `string_match`, e.g. a stray header leak),
  `num_clean` (takes the leading number before a `[` bracket, e.g. `298[110-510]`).

So a typical solution is just:

```
# DA-Bench id N: <task>, expect <answer> #
let data = ^^csv("file.csv") in
round2(mean(num(data.`Column`)))
```

Add task-specific `let`s (e.g. a binning function) as needed; they can reference
the prelude helpers. Unused prelude helpers just produce harmless warnings (`run`
ignores them). To add a broadly-useful helper, put it in `prelude.hz`.

## Reading columns

- **Project a whole column** with `.`: `data.\`Fare\`` is the list of all Fare
  cells. Don't `map` just to extract a column.
- **Backtick every header.** CSV headers are usually capitalized or spaced
  (`Fare`, `Close Price`, `No. of cases`); unquoted Hazel identifiers must start
  lowercase, so always write `` data.`Header` ``. Lowercase headers (e.g.
  insurance.csv `age`) also work backticked.
- Cells are strings, often **quoted and space-padded** (e.g. GODREJIND's
  `"        578.55"`). `float_of_string` rejects whitespace → always
  `string_trim` first. `num` above does trim + drop-blank + parse.
- **Missing values** are empty strings `""`. Filter them (`filter(_, fun s -> s != "")`)
  before parsing, or `float_of_string("")` crashes.
- The unnamed pandas index column (empty header) becomes `\`col0\``.
- For row-aligned work across columns, `zip(colA, colB)` then map — but only if
  neither column has blanks (filtering one column alone misaligns the pair).

## Language gotchas (the tylr surface syntax — same as table.hz/grade.hz)

- **Comments are `#...#`** — `#` is BOTH the open and close delimiter, so a
  comment must contain no internal `#`. A stray `#` closes the comment early and
  turns the rest into code. Example:
  - `# mean fare result # 40.0 +. 2.0`  → `42.0` (fine)
  - `# DA-Bench #0 mean fare # 40.0 +. 2.0`  → the second `#` closes after
    "DA-Bench ", so `0 mean fare` is parsed as code and `# 40.0 +. 2.0 #` becomes
    a new comment → garbage output `0  mean fare`.
  (Other characters — quotes, brackets, apostrophes — are fine inside comments;
  only `#` is special.)
- **NEVER write a two-line comment.** Because `#` both opens AND closes, a comment like
  `# line one\n# line two #` is parsed as: comment `# line one #` (closed by line two's
  leading `#`), then `line two` as CODE. In `prelude.hz` this silently breaks the whole
  `let … in` chain — every downstream `mean`/`num` then prints *unreduced* with no error.
  Keep each comment on ONE line. (This cost a full debugging cycle.)
- **`#` cannot appear in a column name you project.** A header like `#photo` (traj-Osak)
  becomes label `` `#photo` ``, but `data.`#photo`` fails to parse — the `#` starts a
  comment even inside backticks. Such columns are currently unreachable.
- **A UTF-8 BOM on the first header** is stripped at ingestion (fixed in `src/CLI/Csv.re`),
  so `data.`wage`` works for BOM-prefixed files (beauty, gapminder_cleaned, Current_Logan).
- **No trailing commas.** `f(a, b, c,)` parses as a tuple with an extra empty
  slot (a hole), so the call gets the wrong arity and silently gets *stuck*
  (the result prints as an unreduced expression, not an error). Drop the comma.
- **No `not`** — use prefix `!` or `!=`. (`s != ""`, not `not(s == "")`.)
- **Negative float literals**: write `0. -. 2.5`, not `-. 2.5` (the latter parses
  as `¿ -. 2.5`, an empty-hole subtraction).
- Float ops are dotted: `+. -. *. /. **. <. >. <=. >=. ==.`. Int division `/`,
  `int_mod(n, m)`.
- `|>` pipe and `_` placeholder work: `xs |> filter(_, p) |> map(_, f)`.
- `sort(cmp, xs)` takes the comparator first; `cmp` returns `Lt`/`Eq`/`Gt`.
- Results print via `%f` (6 decimals), so `34.65` prints as `34.650000` — match
  by the leading digits.
- Lambdas can destructure tuples: `fun (x, y) -> …`, `fun (a, _f) -> …`.

## Useful builtins

`map filter fold_left length zip sort nth take drop reverse sum?(no—fold)`,
`float_of_string int_of_string string_trim string_split string_match`,
`sqrt abs_float floor ceil round exp log`, `float_of_int int_of_float`,
`group_by_label to_lvs omit_labels pivot_table`. There is **no** built-in
`sum`/`mean`/`median`/`std` — compose them (see prelude). `round : Float -> Float`
(nearest, ties away from zero) was added in `src/language/builtins/BuiltinsBase.re`.

## Matching the reference answer (spec fidelity, not language limits)

These bit us; the language expresses all of them — you just have to pick pandas'
convention:

- **std defaults to sample (`ddof=1`)** in pandas `.std()` → use `sample_std`
  unless the question explicitly says "population" (then `pop_std`). For large n
  the rounded answer can differ by 0.01.
- **Binning boundaries**: e.g. age groups `[0,13) [13,20) [20,60) [60,∞)`
  matched the labels; mind half-open vs closed and fractional values.
- **Rounding**: format usually says "2 decimal places" → `round2`. Use 4-dp where
  asked (`round(x *. 10000.0) /. 10000.0`).

## What is NOT expressible today (when to give up / flag)

- **scipy p-values** — Shapiro-Wilk, Kolmogorov-Smirnov, t-test p-values need
  statistical distribution CDFs. A correlation *coefficient* is fine; its
  `p_value`/`relationship_type` companion is not (needs a t-distribution CDF).
- **sklearn models** — regression coefficients via normal equations are
  expressible, but `train_test_split(random_state=42)` requires reproducing
  numpy's RNG, which we cannot match.
- Skewness/kurtosis (Fisher-Pearson moments) ARE expressible.

Prefer easy/medium "Summary Statistics", "Correlation Analysis" (coefficient
only), "Feature Engineering", "Outlier Detection (IQR/Z-score)", and
"Distribution Analysis" tasks gated on skewness rather than a hypothesis test.

## Performance

The editor parser is slow on large literals. The AST-splice `run`/`analyze` path
avoids this (parses only the skeleton). Don't hand-inline thousands of rows or
`expand`-then-`run`; reference the CSV with `^^csv(...)` and `run`.

**Row-count ceiling (evaluator stack).** Even via AST splice, the evaluator
recurses over list values, so very large columns overflow the stack
(`Stack overflow`). ~900 rows (titanic) is fine; tree.csv at 9796 rows blows up.
This is an interpreter recursion-depth limit, *not* a language-expressiveness
limit — the program is correct, the runtime just can't fold a list that long.
`da551-mean-dbh.hz` is kept as an example but excluded from `test.sh` for this
reason. If a task's dataset is that large, note it as a runtime limit, not a
"can't express it" result.
