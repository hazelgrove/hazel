#!/usr/bin/env node
/* Compare two benchmark JSON files.
 *
 * Usage:
 *   node bench/compare.js base.json head.json              # terminal table + graph
 *   node bench/compare.js base.json head.json --markdown   # GitHub markdown
 *
 * Shared by both the local comparison script and the CI workflow. */

const fs = require('fs');

const args = process.argv.slice(2);
const markdown = args.includes('--markdown');
const files = args.filter(a => !a.startsWith('--'));
const [baseFile, headFile] = files;

if (!baseFile || !headFile) {
  console.error('Usage: node bench/compare.js <base.json> <head.json> [--markdown]');
  process.exit(1);
}

let baseResults = [];
try {
  baseResults = JSON.parse(fs.readFileSync(baseFile, 'utf8'));
} catch (e) {
  console.error(`Warning: could not read base results (${e.message})`);
}

let headResults = [];
try {
  headResults = JSON.parse(fs.readFileSync(headFile, 'utf8'));
} catch (e) {
  console.error(`Warning: could not read head results (${e.message})`);
}

const baseMap = {};
for (const r of baseResults) {
  baseMap[r.name] = r.time_ns;
}

function formatTime(ns) {
  if (ns === undefined || ns === 0 || isNaN(ns)) return '-';
  if (ns >= 1e9) return (ns / 1e9).toFixed(2) + ' s';
  if (ns >= 1e6) return (ns / 1e6).toFixed(2) + ' ms';
  if (ns >= 1e3) return (ns / 1e3).toFixed(2) + ' us';
  return ns.toFixed(0) + ' ns';
}

function formatDelta(base, head) {
  if (!base || base === 0 || isNaN(base)) return 'new';
  if (head === 0 || isNaN(head)) return '-';
  const pct = ((head - base) / base * 100);
  const sign = pct >= 0 ? '+' : '';
  if (markdown) {
    const emoji = pct > 10 ? ' :warning:' : pct < -10 ? ' :rocket:' : '';
    return `${sign}${pct.toFixed(1)}%${emoji}`;
  } else {
    const emoji = pct > 10 ? ' ⚠️' : pct < -10 ? ' 🚀' : '';
    return `${sign}${pct.toFixed(1)}%${emoji}`;
  }
}


const editResults = headResults.filter(r => r.name.includes('/edit/'));
const memoResults = headResults.filter(r => r.name.includes('/memo/'));

/* Compute Total rows by summing individual pipeline phases per program size.
 * Excludes Move(Left) since it's a baseline reference, not part of the pipeline. */
const pipelinePhases = ['Perform', 'MakeTerm', 'Measured', 'Statics', 'Elaborate', 'Evaluate'];
const sizeGroups = {};
for (const r of editResults) {
  const slash = r.name.indexOf('/edit/');
  if (slash < 0) continue;
  const size = r.name.slice(0, slash);
  const phase = r.name.slice(slash + 6);
  if (!pipelinePhases.includes(phase)) continue;
  if (!sizeGroups[size]) sizeGroups[size] = { head: 0, base: 0 };
  sizeGroups[size].head += r.time_ns || 0;
  sizeGroups[size].base += baseMap[r.name] || 0;
}
const totalRows = [];
for (const [size, sums] of Object.entries(sizeGroups)) {
  const name = `${size}/edit/Total`;
  totalRows.push({ name, time_ns: sums.head });
  baseMap[name] = sums.base;
}
const editWithTotals = [...editResults, ...totalRows];

if (markdown) {
  function makeTable(results) {
    let table = '| Benchmark | Base | PR | Delta |\n';
    table += '|:---|---:|---:|---:|\n';
    for (const r of results) {
      const baseTime = baseMap[r.name];
      table += `| \`${r.name}\` | ${formatTime(baseTime)} | ${formatTime(r.time_ns)} | ${formatDelta(baseTime, r.time_ns)} |\n`;
    }
    return table;
  }

  if (headResults.length === 0) {
    console.log(':x: Benchmarks failed to build or run.');
  } else {
    console.log('### Edit Cycle (per-keystroke latency)\n');
    console.log(makeTable(editWithTotals));

    if (memoResults.length > 0) {
      console.log('<details><summary>Memo-hit overhead (repeated calls, same input)</summary>\n');
      console.log(makeTable(memoResults));
      console.log('</details>\n');
    }

    console.log('<details><summary>Legend</summary>\n');
    console.log('- :rocket: = >10% faster');
    console.log('- :warning: = >10% slower');
    console.log('- "new" = benchmark not present on base branch');
    console.log('</details>');
  }
} else {
  // Terminal: table + graph
  function printTable(title, results) {
    if (results.length === 0) return;
    console.log(`\n${title}`);

    const nameWidth = Math.max(10, ...results.map(r => r.name.length));
    const colWidth = 12;

    const header = 'Benchmark'.padEnd(nameWidth) + '  ' +
      'Base'.padStart(colWidth) + '  ' +
      'Head'.padStart(colWidth) + '  ' +
      'Delta'.padStart(colWidth);
    console.log(header);
    console.log('-'.repeat(header.length));

    for (const r of results) {
      const baseTime = baseMap[r.name];
      console.log(
        r.name.padEnd(nameWidth) + '  ' +
        formatTime(baseTime).padStart(colWidth) + '  ' +
        formatTime(r.time_ns).padStart(colWidth) + '  ' +
        formatDelta(baseTime, r.time_ns).padStart(colWidth)
      );
    }
  }

  printTable('Edit Cycle (per-keystroke latency)', editWithTotals);
  printTable('Memo-hit overhead (repeated calls, same input)', memoResults);
  console.log();
}
