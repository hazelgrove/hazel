# Generating the paper's benchmark table

`bench/latex-table.js` turns `./hazel bench-incr --json` output into the row
bodies of `tab:hazel-benchmarks`, the six-scenario timing table in
`papers/incremental evaluation/paper/sections/21_hazel_benchmarking.tex`.

It exists because those numbers used to reach the paper by hand — someone read
a figure off `bench/summarize.js`'s terminal table and retyped it into LaTeX.
We published a stale number that way once.

## Regenerating the table after a benchmark run

```sh
# 1. Re-run the benchmark, writing JSON (not just the .txt summary).
./hazel bench-incr --json=bench/results/paper-six.bench.json

# 2. Regenerate the table.
node bench/latex-table.js bench/results/paper-six.bench.json
```

That prints the whole `table` environment — preamble, rows, caption, label —
ready to replace the existing one in `21_hazel_benchmarking.tex`. As of the
commit that added this script, the output is **byte-for-byte identical** to the
hand-written table, so the first regeneration should be a no-op diff.

`--rows-only` prints just the row bodies, for when you have edited the
preamble or caption by hand and only want the numbers:

```sh
node bench/latex-table.js --rows-only bench/results/paper-six.bench.json
```

You can pass several files. **Later files deliberately win** on a trace they
share with an earlier one, which is the intended way to splice in a single
re-recorded trace without re-running everything:

```sh
node bench/latex-table.js bench/results/paper-six.bench.json \
                          bench/results/tariff-tune.bench.json
```

The override is announced on stderr, and **that note is the thing to watch
for** — it is the only signal distinguishing "I meant to replace that trace"
from "I passed the wrong file and half my numbers just changed":

```
note: tariff-tune appears in both bench/results/paper-six.bench.json and bench/results/tariff-tune.bench.json; using bench/results/tariff-tune.bench.json
```

`--policy=NAME` picks an id policy if a run carries more than one. With a
single policy (today: `default`) you never need it.

Only stdout is the table. Notes, warnings, and errors all go to stderr, so
`node bench/latex-table.js ... > table.tex` is safe.

## Running the checks

```sh
node --test bench/latex-table.test.js
```

Node's built-in runner; no dependencies, nothing to install. Most of the suite
covers the shading rule below, because that is the part the real data cannot
exercise — see the comment at the top of the test file.

Note that `node --test bench/` (the directory form) does not work on Node 26;
name the file.

## Adding or changing a scenario

Edit the `SCENARIOS` list at the top of the script, and nothing else. Scenario
numbers are positions in that list, and each entry holds the trace name (the
basename of the `trace` field in the JSON, minus `.json`) and the prose for the
Scenario column. A seventh scenario is one appended entry.

## What the three `\hazelresult` arguments mean

`\hazelresult{time}{speedup}{extreme}`, defined in
`21_hazel_benchmarking.tex`. The first two are printed; the third only controls
the colour of the second.

1. **`time`** — total incremental re-evaluation time in milliseconds: sum each
   rep's `eval_ms` over every step **except step 0**, then take the **median**
   of those per-rep totals. This is `incr~med` in `bench/summarize.js`. Step 0
   is the cold evaluation, where every calculus starts from an empty cache and
   therefore measures nothing incremental. The sum happens before the median on
   purpose — taking medians per step and summing those would report a total no
   single rep ever ran.

   Median rather than mean is load-bearing, not incidental: with five reps, one
   slow rep moves a mean a long way. Switching to the mean changes all thirty
   published cells, one of them by 76%, and pushes two of scenario 1's cells
   across 1x, inverting that row's shading from brown to blue.

2. **`speedup`** — `a0_time / this_time`, from the same run. Above 1 is faster
   than non-incremental evaluation, below 1 is slower. `a0`'s own cell is
   always exactly `1.00`: it is the baseline. Only the ratio is really
   meaningful; absolute times drift with machine load, but `a0` holds no cache,
   so nothing a calculus does can change it and dividing by it cancels the
   drift.

3. **`extreme`** — a shading normalizer, **not** "the best result in the row",
   which is what it looks like on the current data. The macro shades each cell
   with intensity `min(100, 100 * |(speedup - 1) / (extreme - 1)|)`, blue above
   1x and brown below, and treats `extreme = 1` as "render neutral gray". So it
   is the scale against which this cell's distance from 1x is measured: among
   the four incremental speedups **on the same side of 1x as this cell's own**,
   the one furthest from 1. The most extreme cell on each side saturates and
   the rest scale against it.

   Per side, not per row. Every row in the current data sits entirely on one
   side of 1x, so the two rules agree today — but in a mixed row (say 0.98x
   next to 7x) a single row-wide extreme would render the 0.98x at 0.3%
   intensity, i.e. invisible, when it should read as a mild slowdown. That is
   what the macro's "same side of 1x" wording prevents.

   A cell **alone on its side** has no same-side peer, which the macro's
   wording does not cover. It falls back to the row-wide extreme by distance
   from 1, which may sit on the *other* side of 1x. That is safe because the
   intensity is sign-agnostic (`abs`) and the colour branches on the speedup,
   never on `extreme`, so a cross-side value changes the scale without ever
   changing the hue. It is also the only rule that behaves in both mirror
   images: a lone 9x among three mild slowdowns is itself the row extreme and
   saturates blue, while a lone 0.98x among three large wins scales to ~0.25%
   and reads as neutral.

   Only two things produce the neutral `1.00`: a speedup of exactly 1.00x, and
   a row in which every incremental speedup is exactly 1.00x — matching the
   macro's comment, "a row containing only 1x remains neutral".

Rounding throughout is **half-up to 2 decimals**, on the shortest decimal
string that round-trips the value rather than on the binary double — `toFixed`
breaks exact ties in whichever direction the float happens to sit. No value in
the current data lands within 0.004 of a tie, so this choice does not change
any published number today; it is there so that a future run cannot produce a
digit that depends on floating-point luck.

Speedups are rounded **before** the shading rule sees them, because the macro
compares the printed text (`\ifdim #2pt>1pt`). A cell whose raw ratio is 0.9996
prints `1.00`, and LaTeX will read it as exactly neutral; rounding first keeps
the script's idea of which side of 1x a cell is on identical to LaTeX's.

## It fails instead of guessing

The script would rather print nothing than print a number it is not sure of.
A table that obviously failed gets fixed; one that is quietly wrong gets
published. All of these exit non-zero with an empty stdout and an explanation
on stderr, and all problems are reported at once rather than one per run:

- a trace named in `SCENARIOS` is absent from the JSON;
- a calculus is missing for some trace;
- a rep is missing a step, which would silently understate that rep's total;
- a trace has no step 0, so there is no cold step to exclude;
- the `a0` total is zero or missing, so there is no speedup to take;
- the file is not a `bench-incr --json` file, or will not parse;
- more than one id policy is present and `--policy` was not given.

There is deliberately no "skip the broken row and carry on" path, and no cell
can be emitted as a placeholder or a zero.

## Known hazards and seams

- **A prose edit in the paper gets silently reverted by the next
  regeneration.** The Scenario column wording lives in `SCENARIOS` in this
  script, so if someone rewords a scenario in `21_hazel_benchmarking.tex` — for
  column fit, say — regenerating overwrites it with the old wording, with no
  error and no warning. It is easy to miss in review because the numbers
  legitimately changed in the same diff. **If you regenerate, re-check the
  Scenario prose against the paper, or copy any reword back into `SCENARIOS`.**

  The fix is to move the prose into the `.tex` and have the generator emit only
  numbers and row numbers. That means editing the paper repo, which is a
  human's call, so it has not been done; the recommendation is with the paper's
  author. The prose is isolated in the one `SCENARIOS` table so the move stays
  a small edit.

- **One table, one id policy.** The table has no policy dimension, so a
  multi-policy run needs `--policy`. If policies ever need to appear side by
  side, that is a new column group and a change here.

- **The per-side shading rule is not exercised by any real benchmark run.**
  Every current row lies entirely on one side of 1x, so the byte-exact
  acceptance diff against the paper cannot tell the per-side rule apart from a
  row-wide maximum. `bench/latex-table.test.js` covers the difference with
  synthetic rows. Those rows are constructed, not measured; if a real mixed row
  ever lands, it is worth eyeballing the rendered colours once.
