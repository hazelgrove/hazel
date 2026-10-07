#!/usr/bin/env node
/* Generate the paper's benchmark table from bench-incr JSON.
 *
 *   node bench/latex-table.js bench/results/paper-six-r25-closure-reuse.bench.json
 *   node bench/latex-table.js --rows-only bench/results/*.bench.json
 *
 * Emits the `tab:hazel-benchmarks` table in
 * papers/incremental evaluation/paper/sections/21_hazel_benchmarking.tex,
 * so that re-running the benchmark updates the paper instead of someone
 * reading numbers off a terminal and retyping them. See bench/latex-table.md,
 * and bench/latex-table.test.js for the checks (`node --test bench/`).
 *
 * The one rule this script is built around: it would rather print nothing
 * than print a number it is not sure of. Every missing trace, missing
 * calculus, or short rep is a hard error on stderr with a non-zero exit --
 * never a zero, a placeholder, or a dropped row. A table that obviously
 * failed gets fixed; a table that is quietly wrong gets published.
 */

const fs = require('fs');
const path = require('path');

/* The scenario table. Adding a seventh scenario is one entry here: scenario
 * numbers are positions in this list, and the prose is the table's Scenario
 * column verbatim. Nothing else in this file knows how many scenarios there
 * are or what they are called.
 *
 * The prose lives here under protest -- it is the paper's wording, not the
 * benchmark's, so a reword in the .tex gets silently reverted by the next
 * regeneration. bench/latex-table.md spells out that hazard. Moving it into
 * the .tex means editing the paper repo, which is a human's call; it is
 * isolated here so that move stays a small edit. */
const SCENARIOS = [
  {
    trace: 'reshape-source',
    description:
      'Re-running a report against multiple datasets, all downstream code relies on the dataset and must be recalculated each time.',
  },
  {
    trace: 'add-metrics',
    description:
      'Add three new summary metrics to the end of a dataset analysis report',
  },
  {
    trace: 'fix-middle',
    description: 'Fix two stated bugs in the middle of a dataset analysis report.',
  },
  {
    trace: 'tune-threshold',
    description:
      'Find a threshold value for one of the metrics in the report using a binary search.',
  },
  {
    trace: 'tariff-tune',
    description:
      'Similar to scenario 4, but the report generation is parameterized and thus inside a function, so any re-use would happen inside a function call.',
  },
  {
    trace: 'split-summary',
    description:
      'As part of report generation, three values are packed into a tuple, and then unpacked downstream. The agent is asked to fix bugs in all three values one at a time.',
  },
];

/* Column order of the table. a0 is the baseline and gets special treatment
 * in the shading rule below; the other four are the incremental calculi. */
const BASELINE = 'a0';
const CALCULI = ['a0', 'aPL', 'aM', 'a2', 'aStar'];

/* ---------- numbers ---------- */

/* Round half-up to `places` decimals.
 *
 * Deliberately not `toFixed`, which rounds the underlying double and so
 * breaks decimal ties in whichever direction the binary representation
 * happens to sit: (0.125).toFixed(2) is "0.13" but (1.005).toFixed(2) is
 * "1.00". We round the shortest decimal string that round-trips to x --
 * the number a human reading the JSON would see -- and break ties upward,
 * so the rule is one sentence long and identical on every run. */
function roundHalfUp(x, places = 2) {
  if (typeof x !== 'number' || !Number.isFinite(x)) {
    throw new Error(`refusing to round a non-finite value: ${x}`);
  }
  const s = Math.abs(x).toString();
  if (s.includes('e')) return Number(x.toFixed(places)); // magnitudes we never see
  const [int, frac = ''] = s.split('.');
  if (frac.length <= places) return x;
  let n = BigInt(int + frac.slice(0, places));
  if (frac.charCodeAt(places) - 48 >= 5) n += 1n;
  const r = Number(n) / 10 ** places;
  return x < 0 ? -r : r;
}

const fmt2 = x => roundHalfUp(x, 2).toFixed(2);

function median(xs) {
  const s = [...xs].sort((a, b) => a - b);
  const n = s.length;
  return n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2;
}

/* The table's first \hazelresult argument: total incremental re-evaluation
 * time in ms. `incr~med` in bench/summarize.js's terminal table.
 *
 * Sum each rep's eval_ms over every step but the cold one, then take the
 * median of those per-rep totals. Two choices are baked in here, and this is
 * the only place to change either:
 *
 *   - Median, not mean. The median is what bench-incr and summarize.js
 *     report, and it is what the published numbers were read off. Switching
 *     to the mean moves all thirty cells and one of them by 76%, because a
 *     single slow rep drags a 5-rep mean a long way. It would also push two
 *     of scenario 1's cells across 1x, inverting that row's shading from
 *     brown to blue. Do not switch casually.
 *   - Excluding step 0 (the cold evaluation), where every calculus starts
 *     from an empty cache and so measures nothing incremental.
 *
 * Summing per-step medians instead would report a total no single rep ever
 * exhibited, which is why the sum happens before the median. */
function incrTotalMs(samples) {
  const byRep = new Map();
  for (const m of samples) {
    if (m.step === 0) continue;
    byRep.set(m.rep, (byRep.get(m.rep) || 0) + m.eval_ms);
  }
  return median([...byRep.values()]);
}

const furthestFrom1 = xs =>
  xs.reduce((best, s) => (Math.abs(s - 1) > Math.abs(best - 1) ? s : best));

/* The table's third \hazelresult argument: the shading normalizer.
 *
 * It is not "the best result in the row", which is what it looks like on the
 * current data. From the macro definition in
 * papers/incremental evaluation/paper/sections/21_hazel_benchmarking.tex:
 *
 *   % Arguments: time, speedup, row extreme on the same side of 1x.
 *   % Normalize distance from 1x; a row containing only 1x remains neutral.
 *   \ifdim #3pt=1pt\relax\else
 *     \pgfmathsetmacro{\hazelintensity}{min(100,100*abs((#2-1)/(#3-1)))}%
 *     \ifdim #2pt>1pt  -> hazelFaster (blue)
 *     \ifdim #2pt<1pt  -> hazelSlower (brown)
 *
 * So the cell is shaded with intensity |(speedup - 1) / (arg3 - 1)|, and
 * arg3 = 1 is the sentinel for "render neutral gray" -- reserved, per the
 * macro's own comment, for a row containing only 1x. arg3 is therefore a
 * scale: the speedup furthest from 1 among those on the same side of 1x as
 * this cell, so the most extreme cell on each side saturates and the rest
 * scale against it.
 *
 * Per side, not per row. Every row in the current data sits entirely on one
 * side of 1x, so the two rules agree today -- but a mixed row (0.98x
 * alongside 7x) is the expected shape once real agent traces land, and a
 * single row-wide extreme would crush the shading on whichever side is
 * smaller. That is what the macro's "same side of 1x" wording exists to
 * prevent.
 *
 * The one case the macro's wording does not cover is a cell *alone* on its
 * side, which has no same-side peer to scale against. We fall back to the
 * row-wide extreme by distance from 1, which may sit on the other side of
 * 1x. That is safe because the intensity is sign-agnostic (`abs`) and the
 * colour branches on #2, never #3, so a cross-side arg3 changes the scale
 * without ever changing the hue. It is also the only option that behaves in
 * both mirror images: a lone 9x among three mild slowdowns is itself the row
 * extreme and saturates blue, while a lone 0.98x among three large wins
 * scales to ~0.25% and reads as neutral. Using the arg3 = 1 sentinel here
 * instead would render that lone 9x -- the most dramatic result in the table
 * -- as flat gray.
 *
 * Takes the already-rounded speedups, because the macro compares the printed
 * text: a cell that prints 1.00 must be treated as 1.00 here even if its raw
 * ratio was 0.9996, or LaTeX and this script would disagree about its side.
 *
 * `siblings` is all four incremental speedups in the row, including this
 * cell's own. a0 never comes through here -- see rowFor. */
function sameSideExtreme(speedup, siblings) {
  if (speedup === 1) return 1; // exactly 1.00x: neutral, and no division by zero
  const side = Math.sign(speedup - 1);
  const onSide = siblings.filter(s => Math.sign(s - 1) === side);
  if (onSide.length >= 2) return furthestFrom1(onSide);
  /* Alone on its side. Scale against the row's extreme regardless of side;
   * cells at exactly 1.00x are excluded so this can never return the
   * neutral sentinel by accident, nor divide by zero. */
  const scalable = siblings.filter(s => s !== 1);
  return scalable.length === 0 ? 1 : furthestFrom1(scalable);
}

/* ---------- loading ---------- */

const traceName = t => path.basename(t).replace(/\.json$/, '');

/* trace name -> { samples, file }. Later files win on a collision, so that
 * re-recording one trace is `latex-table.js paper-six-r25-closure-reuse.bench.json
 * that-trace.bench.json`. That override is deliberate and the stderr note is
 * the thing to watch for: a silent override is how a stale number gets
 * published. */
function loadTraces(files, problems) {
  const byTrace = new Map();
  const policies = new Set();

  for (const file of files) {
    let data;
    try {
      data = JSON.parse(fs.readFileSync(file, 'utf8'));
    } catch (e) {
      problems.push(`cannot read ${file}: ${e.message}`);
      continue;
    }
    if (!Array.isArray(data.samples)) {
      problems.push(`${file}: no "samples" array -- is this a bench-incr --json file?`);
      continue;
    }
    for (const m of data.samples) policies.add(m.policy);

    const seenHere = new Map();
    for (const m of data.samples) {
      const name = traceName(m.trace);
      if (!seenHere.has(name)) seenHere.set(name, []);
      seenHere.get(name).push(m);
    }
    for (const [name, samples] of seenHere) {
      const prev = byTrace.get(name);
      if (prev) {
        console.error(`note: ${name} appears in both ${prev.file} and ${file}; using ${file}`);
      }
      byTrace.set(name, { samples, file });
    }
  }
  return { byTrace, policies: [...policies] };
}

/* ---------- one row per scenario ---------- */

function rowFor(scenario, number, ctx) {
  const { byTrace, policy, files, problems } = ctx;
  const fail = msg => problems.push(`scenario ${number} (${scenario.trace}): ${msg}`);

  const entry = byTrace.get(scenario.trace);
  if (!entry) {
    problems.push(
      `scenario ${number} (${scenario.trace}): no samples for this trace in ${files.join(', ')}`
    );
    return null;
  }
  const traceSamples = entry.samples.filter(m => m.policy === policy);
  if (traceSamples.length === 0) {
    fail(`no samples under policy "${policy}"`);
    return null;
  }

  const allSteps = [...new Set(traceSamples.map(m => m.step))].sort((a, b) => a - b);
  if (!allSteps.includes(0)) {
    fail(
      'no step 0, so there is no cold evaluation to exclude; the totals would ' +
        'not mean what the table says'
    );
    return null;
  }
  if (allSteps.length < 2) {
    fail('only the cold step, no edits to time');
    return null;
  }

  const times = new Map();
  let short = false;
  for (const calculus of CALCULI) {
    const ms = traceSamples.filter(m => m.calculus === calculus);
    if (ms.length === 0) {
      fail(`calculus ${calculus} is missing entirely`);
      short = true;
      continue;
    }
    /* A rep missing a step would silently understate that rep's total and
     * could drag the median with it. Catch it rather than publish it. */
    const stepsByRep = new Map();
    for (const m of ms) {
      if (!stepsByRep.has(m.rep)) stepsByRep.set(m.rep, new Set());
      stepsByRep.get(m.rep).add(m.step);
    }
    for (const [rep, steps] of [...stepsByRep].sort((a, b) => a[0] - b[0])) {
      const missing = allSteps.filter(s => !steps.has(s));
      if (missing.length > 0) {
        fail(
          `${calculus} rep ${rep} is missing step(s) ${missing.join(', ')} ` +
            `of ${allSteps.join(', ')}`
        );
        short = true;
      }
    }
    times.set(calculus, incrTotalMs(ms));
  }
  if (short) return null;

  const base = times.get(BASELINE);
  if (!(base > 0)) {
    fail(`${BASELINE} total is ${base}, so there is nothing to take a speedup against`);
    return null;
  }

  /* Speedups, rounded before the shading rule sees them. a0's own cell is
   * the baseline: exactly 1.00x and deliberately neutral, so its arg3 is not
   * computed from the row. */
  const speedups = new Map(CALCULI.map(c => [c, roundHalfUp(base / times.get(c), 2)]));
  const incremental = CALCULI.filter(c => c !== BASELINE).map(c => speedups.get(c));

  return {
    number,
    description: scenario.description,
    edits: allSteps.length - 1,
    cells: CALCULI.map(calculus => {
      const speedup = speedups.get(calculus);
      const extreme = calculus === BASELINE ? 1 : sameSideExtreme(speedup, incremental);
      return `\\hazelresult{${fmt2(times.get(calculus))}}{${fmt2(speedup)}}{${fmt2(extreme)}}`;
    }),
  };
}

/* ---------- output ---------- */

const PREAMBLE = `\\begin{table}
  \\centering
  \\small
  \\setlength{\\tabcolsep}{3pt}
  \\begin{tabularx}{\\linewidth}{@{}c p{0.30\\linewidth} c RRRRR@{}}
    \\toprule
    \\# & Scenario & Edits &
      \\aZero{} &
      $\\ap{}+\\al{}$ &
      $\\cdots+\\,\\am{}$ &
      $\\cdots+\\,\\aC{}$ &
      \\astar{} \\\\
    \\midrule`;

const POSTAMBLE = `    \\bottomrule
  \\end{tabularx}
  \\caption{Total evaluation time (ms), and speedup relative to regular evaluation \\aZero{} across all edits made in each benchmarking scenario. Columns $\\cdots+\\,\\am{}$ and $\\cdots+\\,\\aC{}$ abbreviate $(\\ap{}+\\al{})+\\am{}$ and $(\\ap{}+\\al{})+\\aC{}$ respectively.}
  \\label{tab:hazel-benchmarks}
\\end{table}`;

function renderRows(rows) {
  return rows
    .map(r => `    ${r.number} & ${r.description}\n      & ${r.edits} & ${r.cells.join(' & ')} \\\\`)
    .join('\n    \\addlinespace\n');
}

const renderTable = rows => `${PREAMBLE}\n${renderRows(rows)}\n${POSTAMBLE}`;

/* ---------- cli ---------- */

const USAGE = [
  'usage: node bench/latex-table.js [--rows-only] [--policy=NAME] <results.bench.json>...',
  '  --rows-only   emit just the row bodies, for pasting into an existing table',
  '  --policy=N    pick an id policy when the JSON carries more than one',
];

function main(args) {
  const files = args.filter(a => !a.startsWith('--'));
  const rowsOnly = args.includes('--rows-only');
  const policyFlag = (args.find(a => a.startsWith('--policy=')) || '').split('=')[1];
  const wantsHelp = args.includes('--help');
  const unknown = args.filter(
    a => a.startsWith('--') && a !== '--rows-only' && a !== '--help' && !a.startsWith('--policy=')
  );

  if (wantsHelp || files.length === 0 || unknown.length > 0) {
    for (const u of unknown) console.error(`unknown flag: ${u}`);
    for (const line of USAGE) console.error(line);
    return wantsHelp && unknown.length === 0 ? 0 : 1;
  }

  const problems = [];
  const { byTrace, policies } = loadTraces(files, problems);
  if (problems.length > 0) {
    for (const p of problems) console.error(`error: ${p}`);
    return 1;
  }

  /* One id policy per table, since the table has one row per scenario and no
   * room for a policy dimension. The paper-six runs carry only "default". */
  let policy = policyFlag;
  if (policy === undefined) {
    if (policies.length === 1) {
      policy = policies[0];
    } else {
      console.error(
        `error: ${policies.length} id policies in the input (${policies.join(', ')}); ` +
          'pick one with --policy=NAME'
      );
      return 1;
    }
  } else if (!policies.includes(policy)) {
    console.error(`error: no samples with policy "${policy}" (have: ${policies.join(', ')})`);
    return 1;
  }

  const ctx = { byTrace, policy, files, problems };
  const rows = SCENARIOS.map((s, i) => rowFor(s, i + 1, ctx)).filter(Boolean);

  if (problems.length > 0) {
    for (const p of problems) console.error(`error: ${p}`);
    console.error(`\n${problems.length} problem(s); refusing to emit a partial table.`);
    return 1;
  }

  console.log(rowsOnly ? renderRows(rows) : renderTable(rows));
  return 0;
}

if (require.main === module) {
  process.exit(main(process.argv.slice(2)));
}

module.exports = {
  BASELINE,
  CALCULI,
  SCENARIOS,
  incrTotalMs,
  main,
  median,
  renderRows,
  renderTable,
  roundHalfUp,
  sameSideExtreme,
};
