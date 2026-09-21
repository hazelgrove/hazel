#!/usr/bin/env node
/* Cross-task summary of bench-incr runs.
 *
 *   node bench/summarize.js bench/results/*.bench.json
 *
 * One row per (task, calculus), reporting the incremental total -- eval time
 * summed over every step but the cold one -- as a ratio against a0 measured
 * in the same run. The ratio is the number worth reading: absolute times
 * drift with machine load, but a0 holds no cache, so nothing a calculus does
 * can causally change it, and dividing by it cancels the drift.
 *
 * Ratios below 1 are speedups (0.10 = 10x faster than not caching). At or
 * above 1 the scheme lost to doing nothing, which is the correct and
 * expected outcome on a trace whose edits invalidate everything.
 */

const fs = require('fs');
const path = require('path');

const files = process.argv.slice(2).filter(a => !a.startsWith('--'));
if (files.length === 0) {
  console.error('usage: node bench/summarize.js <results.bench.json>...');
  process.exit(1);
}

/* Median of per-rep incremental totals, matching what bench-incr's own
 * summary reports: sum each rep's non-cold steps, then take the median of
 * those totals. Summing the per-step medians instead would report a quantity
 * no single run ever exhibited. */
function median(xs) {
  if (xs.length === 0) return NaN;
  const s = [...xs].sort((a, b) => a - b);
  const n = s.length;
  return n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2;
}

function incrTotals(samples, policy, calculus) {
  const byRep = new Map();
  for (const m of samples) {
    if (m.policy !== policy || m.calculus !== calculus) continue;
    if (m.step === 0) continue; // cold: every calculus starts from an empty cache
    byRep.set(m.rep, (byRep.get(m.rep) || 0) + m.eval_ms);
  }
  return [...byRep.values()];
}

const rows = [];
for (const file of files) {
  let data;
  try {
    data = JSON.parse(fs.readFileSync(file, 'utf8'));
  } catch (e) {
    console.error(`skipping ${file}: ${e.message}`);
    continue;
  }
  const task = path.basename(file).replace(/\.bench\.json$/, '');
  const samples = data.samples || [];
  const policies = [...new Set(samples.map(m => m.policy))];
  const calculi = [...new Set(samples.map(m => m.calculus))];
  const steps = new Set(samples.map(m => m.step)).size;

  for (const policy of policies) {
    const base = median(incrTotals(samples, policy, 'a0'));
    for (const calculus of calculi) {
      const med = median(incrTotals(samples, policy, calculus));
      rows.push({
        task,
        steps: steps - 1, // exclude the cold step
        policy,
        calculus,
        ms: med,
        ratio: base > 0 ? med / base : NaN,
      });
    }
  }
}

const fmt = (x, d = 2) => (isNaN(x) ? '-' : x.toFixed(d));
const speedup = r => (isNaN(r) || r <= 0 ? '-' : r < 1 ? `${(1 / r).toFixed(1)}x faster` : `${r.toFixed(2)}x SLOWER`);

const w = { task: 16, cal: 8, pol: 10 };
console.log();
console.log(
  'task'.padEnd(w.task) + '  ' + 'edits'.padStart(5) + '  ' +
  'calculus'.padEnd(w.cal) + '  ' + 'policy'.padEnd(w.pol) + '  ' +
  'incr~med ms'.padStart(12) + '  ' + 'vs a0'.padStart(7) + '  ' + 'reading'
);
console.log('-'.repeat(88));

let lastTask = null;
for (const r of rows) {
  if (lastTask !== null && r.task !== lastTask) console.log();
  lastTask = r.task;
  console.log(
    r.task.padEnd(w.task) + '  ' +
    String(r.steps).padStart(5) + '  ' +
    r.calculus.padEnd(w.cal) + '  ' +
    r.policy.padEnd(w.pol) + '  ' +
    fmt(r.ms).padStart(12) + '  ' +
    fmt(r.ratio, 3).padStart(7) + '  ' +
    (r.calculus === 'a0' ? '(baseline: no caching)' : speedup(r.ratio))
  );
}
console.log();
console.log('vs a0 below 1.0 = faster than not caching. At or above 1.0 = caching lost.');
console.log('Totals exclude the cold first step, where every calculus starts empty.');
console.log();
