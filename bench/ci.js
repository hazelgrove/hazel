#!/usr/bin/env node
/* Confidence intervals for bench-incr results.
 *
 *   node bench/ci.js bench/results/paper-six.bench.json
 *   node bench/ci.js --target=0.95 results.json
 *
 * Why this exists. `latex-table.js` reports the median total re-evaluation
 * time per cell, which is the published number. It says nothing about how
 * well that median is pinned down, and at small rep counts it is pinned down
 * much less well than two decimal places of printed output suggest. This
 * script reports the interval, and for the pairwise claims the paper actually
 * makes ("aM beats aPL on split-summary") it reports whether the interval
 * clears 1.0 at all.
 *
 * Method. The median's confidence interval is taken from order statistics,
 * which assumes nothing about the shape of the distribution -- appropriate
 * here, because the per-rep totals are badly right-skewed by occasional
 * garbage-collection and JIT pauses that run 2-14x the typical pass. For a
 * sample of n sorted values, the interval [x_(i), x_(n+1-i)] covers the true
 * median with probability
 *
 *     sum_{j=i}^{n-i} C(n,j) / 2^n
 *
 * The tightest i meeting the target coverage is chosen. Note that coverage is
 * bounded by the sample size: at n=5 the widest possible interval, [min,max],
 * reaches only 93.75%, so a 95% interval does not exist and the script says so
 * rather than quietly reporting a narrower one.
 *
 * Pairwise ratios are computed WITHIN each rep before being summarised, so
 * that drift shared by both calculi in that rep cancels. This matters less
 * than it sounds, because the dominant noise is sporadic per-pass spikes
 * rather than smooth drift, but it cannot hurt.
 *
 * Totals exclude step 0, the cold evaluation, matching latex-table.js. */

const fs = require('fs');

const args = process.argv.slice(2);
let target = 0.95;
const files = [];
for (const a of args) {
  const m = /^--target=([\d.]+)$/.exec(a);
  if (m) target = parseFloat(m[1]);
  else if (a === '--help' || a === '-h') {
    console.log('usage: node bench/ci.js [--target=0.95] RESULTS.json...');
    process.exit(0);
  } else if (a.startsWith('--')) {
    console.error(`error: unknown flag ${a}`);
    process.exit(1);
  } else files.push(a);
}
if (files.length === 0) {
  console.error('error: no results file given; see --help');
  process.exit(1);
}

const CAL = ['a0', 'aPL', 'aM', 'a2', 'aStar'];
const BASE = 'a0';

const median = a => {
  const s = [...a].sort((x, y) => x - y), m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};
const choose = (n, k) => { let r = 1; for (let i = 0; i < k; i++) r = r * (n - i) / (i + 1); return r; };

/* Coverage of [x_(i), x_(n+1-i)], with i 1-based. */
const coverageOf = (n, i) => {
  let s = 0;
  for (let j = i; j <= n - i; j++) s += choose(n, j);
  return s / 2 ** n;
};

/* Tightest order-statistic interval reaching `target`, or the widest
 * available when the sample is too small to reach it at all. */
function medianCI(xs, target) {
  const s = [...xs].sort((a, b) => a - b), n = s.length;
  if (n < 2) return { lo: s[0], hi: s[0], coverage: 0, reachable: false };
  let best = null;
  for (let i = 1; i <= n >> 1; i++) {
    const c = coverageOf(n, i);
    if (c >= target) best = { i, c }; else break;
  }
  const reachable = best !== null;
  const { i, c } = best ?? { i: 1, c: coverageOf(n, 1) };
  return { lo: s[i - 1], hi: s[n - i], coverage: c, reachable };
}

/* ---------- load: trace -> calculus -> rep -> total ms, cold excluded ---------- */

const tot = new Map();
for (const f of files) {
  let d;
  try { d = JSON.parse(fs.readFileSync(f, 'utf8')); }
  catch (e) { console.error(`error: cannot read ${f}: ${e.message}`); process.exit(1); }
  if (!Array.isArray(d.samples)) {
    console.error(`error: ${f} has no "samples" array; is it a bench-incr --json file?`);
    process.exit(1);
  }
  for (const s of d.samples) {
    if (s.step === 0) continue;
    const t = s.trace.replace(/.*\//, '').replace(/\.json$/, '');
    if (!tot.has(t)) tot.set(t, new Map());
    const byCal = tot.get(t);
    if (!byCal.has(s.calculus)) byCal.set(s.calculus, new Map());
    const byRep = byCal.get(s.calculus);
    byRep.set(s.rep, (byRep.get(s.rep) || 0) + s.eval_ms);
  }
}
if (tot.size === 0) { console.error('error: no samples found'); process.exit(1); }

const reps = [...tot.values()][0].get(BASE)?.size ?? 0;
const probe = medianCI(Array.from({ length: reps }, (_, i) => i), target);
console.log(`n = ${reps} reps per cell; target coverage ${(100 * target).toFixed(0)}%`);
if (!probe.reachable) {
  console.log(
    `WARNING: ${(100 * target).toFixed(0)}% is UNREACHABLE at n=${reps}. The widest ` +
    `interval, [min,max], covers only ${(100 * probe.coverage).toFixed(2)}%. ` +
    `Intervals below are the widest available, not ${(100 * target).toFixed(0)}%.`
  );
} else {
  console.log(`intervals are order statistics with ${(100 * probe.coverage).toFixed(2)}% coverage`);
}

/* ---------- speedups against the baseline ---------- */

console.log('\nSPEEDUP vs ' + BASE + ' (per-rep ratios)');
console.log('scenario          calculus   median        interval          width/med');
for (const [t, byCal] of tot) {
  const base = byCal.get(BASE);
  for (const c of CAL) {
    if (c === BASE || !byCal.has(c)) continue;
    const x = byCal.get(c);
    const r = [...base.keys()].filter(k => x.has(k)).map(k => base.get(k) / x.get(k));
    const m = median(r), { lo, hi } = medianCI(r, target);
    console.log(
      t.padEnd(18) + c.padEnd(10) + m.toFixed(2).padStart(6) + 'x  ' +
      `[${lo.toFixed(2)}, ${hi.toFixed(2)}]`.padEnd(20) + ((hi - lo) / m).toFixed(2).padStart(7)
    );
  }
}

/* ---------- pairwise claims: does the interval clear 1.0? ---------- */

console.log('\nPAIRWISE (X vs Y = Y is this many times faster than X)');
console.log('scenario          claim           median        interval          verdict');
for (const [t, byCal] of tot) {
  for (const [x, y] of [['aPL', 'aM'], ['aPL', 'a2'], ['aPL', 'aStar']]) {
    if (!byCal.has(x) || !byCal.has(y)) continue;
    const X = byCal.get(x), Y = byCal.get(y);
    const r = [...X.keys()].filter(k => Y.has(k)).map(k => X.get(k) / Y.get(k));
    const m = median(r), { lo, hi } = medianCI(r, target);
    const verdict = lo > 1 ? `${y} faster` : hi < 1 ? `${y} SLOWER` : 'inconclusive (spans 1.0)';
    console.log(
      t.padEnd(18) + `${y} vs ${x}`.padEnd(16) + m.toFixed(2).padStart(6) + 'x  ' +
      `[${lo.toFixed(2)}, ${hi.toFixed(2)}]`.padEnd(20) + verdict
    );
  }
}

/* ---------- skew, as the diagnostic for how much the median is hiding ---------- */

console.log('\nSKEW (mean/median per cell; >1 means right-skewed by pauses)');
console.log('calculus   ' + [...tot.keys()].map(t => t.slice(0, 9).padStart(10)).join(''));
for (const c of CAL) {
  const row = [...tot.keys()].map(t => {
    const x = tot.get(t).get(c);
    if (!x) return ''.padStart(10);
    const v = [...x.values()], mean = v.reduce((a, b) => a + b, 0) / v.length;
    return (mean / median(v)).toFixed(3).padStart(10);
  });
  console.log(c.padEnd(11) + row.join(''));
}
