#!/usr/bin/env node
/* Checks for bench/latex-table.js.
 *
 *   node --test bench/
 *
 * Node's built-in runner, no dependencies, same as the bench scripts.
 *
 * The input is bench/fixtures/six-scenarios-r5.bench.json: a frozen 5-rep run,
 * kept deliberately outside bench/results so that re-running the benchmark
 * never churns the tests, and so nobody regenerating the paper mistakes it for
 * the current numbers. It is a shape fixture -- nothing here asserts its
 * values, and any valid six-scenario run would serve.
 *
 * The weight is deliberately on sameSideExtreme, because the real data cannot
 * exercise it: all six rows sit entirely on one side of 1x, so `onSide` always
 * has four members and the interesting branches never execute. Only a
 * synthetic row can reach them, and a mixed row is the expected shape once
 * real agent traces land.
 */

const test = require('node:test');
const assert = require('node:assert');
const { execFileSync } = require('node:child_process');
const path = require('node:path');

const {
  CALCULI,
  SCENARIOS,
  incrTotalMs,
  median,
  renderRows,
  roundHalfUp,
  sameSideExtreme,
} = require('./latex-table.js');

const SCRIPT = path.join(__dirname, 'latex-table.js');
const PAPER_SIX = path.join(__dirname, 'fixtures', 'six-scenarios-r5.bench.json');

/* What the macro does with the two printed numbers, so the tests can assert
 * on rendered intensity rather than just on arg3. Mirrors
 * \pgfmathsetmacro{\hazelintensity}{min(100,100*abs((#2-1)/(#3-1)))} with
 * the arg3 = 1 sentinel short-circuited to "no colour". */
const intensity = (speedup, extreme) =>
  extreme === 1 ? 0 : Math.min(100, 100 * Math.abs((speedup - 1) / (extreme - 1)));

const extremesFor = row => row.map(s => sameSideExtreme(s, row));

test('sameSideExtreme: all four below 1x (scenario 1 today)', () => {
  const row = [0.98, 0.97, 0.95, 0.93];
  assert.deepStrictEqual(extremesFor(row), [0.93, 0.93, 0.93, 0.93]);
  // The furthest-from-1 cell saturates; the mildest is faintest but visible.
  assert.strictEqual(intensity(0.93, 0.93), 100);
  assert.ok(intensity(0.98, 0.93) > 20);
});

test('sameSideExtreme: all four above 1x (scenario 6 today)', () => {
  const row = [1.76, 3.94, 1.48, 3.94];
  assert.deepStrictEqual(extremesFor(row), [3.94, 3.94, 3.94, 3.94]);
  assert.strictEqual(intensity(3.94, 3.94), 100);
});

test('sameSideExtreme: mixed 2/2 scales each side against its own extreme', () => {
  const row = [0.98, 8.66, 0.9, 4.85];
  assert.deepStrictEqual(extremesFor(row), [0.9, 8.66, 0.9, 8.66]);
  /* The point of per-side: against its own side the mild 0.98x reads as a
   * real slowdown. A single row-wide extreme of 8.66 would put it at 0.3%,
   * indistinguishable from neutral. */
  assert.ok(intensity(0.98, 0.9) > 15);
  assert.ok(intensity(0.98, 8.66) < 1);
});

test('sameSideExtreme: a lone winner saturates rather than going neutral', () => {
  /* Three mild slowdowns and one big win -- the shape scenarios 5 and 6 are
   * designed to produce. The lone 9.00x has no same-side peer, so it falls
   * back to the row extreme, which is itself, and saturates blue. Returning
   * the arg3 = 1 sentinel here would render the most dramatic result in the
   * table as flat gray while the mild losses around it rendered full brown. */
  const row = [0.9, 0.95, 0.98, 9.0];
  assert.deepStrictEqual(extremesFor(row), [0.9, 0.9, 0.9, 9.0]);
  assert.strictEqual(intensity(9.0, 9.0), 100);
  assert.notStrictEqual(sameSideExtreme(9.0, row), 1);
});

test('sameSideExtreme: a lone loser stays visually quiet', () => {
  /* The mirror case, and the reason the fallback is the row extreme rather
   * than the cell itself: a lone 0.98x must not saturate brown just because
   * it is alone. Scaling against the row extreme leaves it near-neutral. */
  const row = [0.98, 8.66, 4.85, 7.0];
  assert.deepStrictEqual(extremesFor(row), [8.66, 8.66, 8.66, 8.66]);
  assert.ok(intensity(0.98, 8.66) < 1);
});

test('sameSideExtreme: exactly 1.00x is neutral', () => {
  assert.strictEqual(sameSideExtreme(1, [1, 8.66, 4.85, 2.0]), 1);
  assert.strictEqual(intensity(1, 1), 0);
});

test('sameSideExtreme: a row of nothing but 1.00x stays neutral', () => {
  // The macro's own comment: "a row containing only 1x remains neutral".
  const row = [1, 1, 1, 1];
  assert.deepStrictEqual(extremesFor(row), [1, 1, 1, 1]);
});

test('sameSideExtreme: lone cell among neutrals never returns the sentinel', () => {
  /* Guards the division by zero the sentinel exists to prevent: the fallback
   * excludes 1.00x cells, so arg3 cannot come back as 1 for a cell that
   * actually wants colour. */
  assert.strictEqual(sameSideExtreme(9.0, [1, 1, 1, 9.0]), 9.0);
  assert.strictEqual(sameSideExtreme(0.5, [1, 1, 1, 0.5]), 0.5);
});

test('roundHalfUp: breaks exact decimal ties upward', () => {
  assert.strictEqual(roundHalfUp(1.005, 2).toFixed(2), '1.01'); // toFixed gives 1.00
  assert.strictEqual(roundHalfUp(8.655, 2).toFixed(2), '8.66'); // toFixed gives 8.65
  assert.strictEqual(roundHalfUp(2.675, 2).toFixed(2), '2.68'); // toFixed gives 2.67
  assert.strictEqual(roundHalfUp(0.125, 2).toFixed(2), '0.13');
  assert.strictEqual(roundHalfUp(-1.005, 2).toFixed(2), '-1.01');
  // Below a tie it must still round down.
  assert.strictEqual(roundHalfUp(1.0049999, 2).toFixed(2), '1.00');
  // Already short enough to leave alone.
  assert.strictEqual(roundHalfUp(442.19, 2), 442.19);
  assert.throws(() => roundHalfUp(NaN, 2), /non-finite/);
  assert.throws(() => roundHalfUp(Infinity, 2), /non-finite/);
});

test('median', () => {
  assert.strictEqual(median([3, 1, 2]), 2); // odd: middle, order-independent
  assert.strictEqual(median([1, 2, 3, 4]), 2.5); // even: mean of the middle pair
});

test('incrTotalMs: median of per-rep sums, excluding the cold step', () => {
  const s = (rep, step, eval_ms) => ({ rep, step, eval_ms });
  const samples = [
    s(0, 0, 1000), s(0, 1, 10), s(0, 2, 20), // rep 0 -> 30
    s(1, 0, 1000), s(1, 1, 50), s(1, 2, 50), // rep 1 -> 100
    s(2, 0, 1000), s(2, 1, 20), s(2, 2, 20), // rep 2 -> 40
  ];
  // Cold step 0 is 1000ms each and must not appear; median of 30/100/40 is 40.
  assert.strictEqual(incrTotalMs(samples), 40);
  /* Summing per-step medians would give 20 + 20 = 40 here by coincidence but
   * is wrong in general: it reports a total no single rep ever ran. Flip one
   * rep to make the two disagree. */
  const skewed = [s(0, 0, 9), s(0, 1, 1), s(1, 0, 9), s(1, 1, 100)];
  assert.strictEqual(incrTotalMs(skewed), 50.5);
});

test('renderRows: format is one row per scenario with \\addlinespace between', () => {
  const rows = [
    { number: 1, description: 'First.', edits: 3, cells: ['A', 'B'] },
    { number: 2, description: 'Second.', edits: 4, cells: ['C', 'D'] },
  ];
  assert.strictEqual(
    renderRows(rows),
    '    1 & First.\n      & 3 & A & B \\\\\n    \\addlinespace\n    2 & Second.\n      & 4 & C & D \\\\'
  );
});

/* ---------- end to end ---------- */

const run = (args, opts = {}) =>
  execFileSync(process.execPath, [SCRIPT, ...args], { encoding: 'utf8', ...opts });

const parseCells = text =>
  text.split('\n').reduce((rows, line) => {
    const cells = [...line.matchAll(/\\hazelresult\{([^}]*)\}\{([^}]*)\}\{([^}]*)\}/g)];
    if (cells.length > 0) rows.push(cells.map(c => [c[1], c[2], c[3]]));
    return rows;
  }, []);

test('end to end: paper-six yields one well-formed row per scenario', () => {
  const rows = parseCells(run(['--rows-only', PAPER_SIX]));
  assert.strictEqual(rows.length, SCENARIOS.length);
  for (const cells of rows) {
    assert.strictEqual(cells.length, CALCULI.length);
    // Every argument is a plain 2-decimal number: no placeholders, no zeros.
    for (const args of cells) {
      for (const a of args) assert.match(a, /^\d+\.\d\d$/);
      assert.ok(Number(args[0]) > 0, `time should be positive, got ${args[0]}`);
      assert.ok(Number(args[1]) > 0, `speedup should be positive, got ${args[1]}`);
    }
  }
});

test('end to end: the a0 column is always the neutral 1.00x baseline', () => {
  for (const cells of parseCells(run(['--rows-only', PAPER_SIX]))) {
    const [, speedup, extreme] = cells[CALCULI.indexOf('a0')];
    assert.strictEqual(speedup, '1.00');
    assert.strictEqual(extreme, '1.00');
  }
});

test('end to end: every emitted arg3 agrees with sameSideExtreme', () => {
  /* Guards the wiring between the rule and the cells, which the byte-exact
   * acceptance diff cannot distinguish from a row-wide maximum. */
  for (const cells of parseCells(run(['--rows-only', PAPER_SIX]))) {
    const incremental = cells.slice(1).map(c => Number(c[1]));
    cells.slice(1).forEach(([, speedup, extreme]) => {
      const want = sameSideExtreme(Number(speedup), incremental);
      assert.strictEqual(Number(extreme), want);
    });
  }
});

test('end to end: the default output is a complete table environment', () => {
  const out = run([PAPER_SIX]);
  assert.match(out, /^\\begin\{table\}/);
  assert.match(out, /\\label\{tab:hazel-benchmarks\}/);
  assert.match(out.trimEnd(), /\\end\{table\}$/);
  assert.ok(out.includes('\\toprule') && out.includes('\\bottomrule'));
  // --rows-only is the same rows without the scaffolding.
  assert.ok(out.includes(run(['--rows-only', PAPER_SIX]).trimEnd()));
});

test('end to end: a missing trace fails loudly and emits nothing', () => {
  /* The property that matters most: no partial table, ever. Dropping a trace
   * from the input must not quietly produce a five-row table. */
  const fs = require('node:fs');
  const os = require('node:os');
  const data = JSON.parse(fs.readFileSync(PAPER_SIX, 'utf8'));
  const dropped = SCENARIOS[SCENARIOS.length - 1].trace;
  data.samples = data.samples.filter(m => !m.trace.includes(dropped));
  const tmp = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'latex-table-')), 'cut.json');
  fs.writeFileSync(tmp, JSON.stringify(data));

  assert.throws(
    () => run([tmp], { stdio: ['ignore', 'pipe', 'pipe'] }),
    e => {
      assert.strictEqual(e.status, 1, 'should exit non-zero');
      assert.strictEqual(e.stdout, '', 'should emit no table at all');
      assert.match(e.stderr, new RegExp(`no samples for this trace`));
      return true;
    }
  );
});

test('end to end: --help exits 0, a bad flag exits 1', () => {
  run(['--help'], { stdio: ['ignore', 'pipe', 'pipe'] }); // throws on non-zero
  assert.throws(
    () => run(['--nope', PAPER_SIX], { stdio: ['ignore', 'pipe', 'pipe'] }),
    e => e.status === 1 && /unknown flag/.test(e.stderr)
  );
});
