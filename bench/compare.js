#!/usr/bin/env node
/* Compare two benchmark JSON files.
 *
 * Usage:
 *   node bench/compare.js base.json head.json              # terminal table
 *   node bench/compare.js base.json head.json --markdown   # GitHub markdown
 *
 * Expects benchmark names in the format: {size}/{scenario}/{phase}
 * where scenario is one of: cold, warm, move, modify
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

/* Group results by scenario */
const scenarios = ['cold', 'warm', 'move', 'modify'];
const scenarioLabels = {
  cold: 'Cold (first run, empty caches)',
  warm: 'Warm (repeated call, cached)',
  move: 'Move (cursor movement, incremental update)',
  modify: 'Modify (content edit, incremental update)',
};

function getScenario(name) {
  for (const s of scenarios) {
    if (name.includes('/' + s + '/')) return s;
  }
  return null;
}

function makeMarkdownTable(results) {
  let table = '| Benchmark | Base | PR | Delta |\n';
  table += '|:---|---:|---:|---:|\n';
  for (const r of results) {
    const baseTime = baseMap[r.name];
    table += `| \`${r.name}\` | ${formatTime(baseTime)} | ${formatTime(r.time_ns)} | ${formatDelta(baseTime, r.time_ns)} |\n`;
  }
  return table;
}

function printTerminalTable(title, results) {
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

if (headResults.length === 0) {
  if (markdown) {
    console.log(':x: Benchmarks failed to build or run.');
  } else {
    console.log('No benchmark results to compare.');
  }
  process.exit(0);
}

if (markdown) {
  for (const scenario of scenarios) {
    const results = headResults.filter(r => getScenario(r.name) === scenario);
    if (results.length === 0) continue;

    const label = scenarioLabels[scenario] || scenario;
    if (scenario === 'cold' || scenario === 'modify') {
      /* Primary scenarios shown expanded */
      console.log(`### ${label}\n`);
      console.log(makeMarkdownTable(results));
    } else {
      /* Secondary scenarios collapsed */
      console.log(`<details><summary>${label}</summary>\n`);
      console.log(makeMarkdownTable(results));
      console.log('</details>\n');
    }
  }

  /* Any results that don't match a known scenario */
  const other = headResults.filter(r => getScenario(r.name) === null);
  if (other.length > 0) {
    console.log('<details><summary>Other</summary>\n');
    console.log(makeMarkdownTable(other));
    console.log('</details>\n');
  }

  console.log('<details><summary>Legend</summary>\n');
  console.log('- :rocket: = >10% faster');
  console.log('- :warning: = >10% slower');
  console.log('- "new" = benchmark not present on base branch');
  console.log('</details>');
} else {
  for (const scenario of scenarios) {
    const results = headResults.filter(r => getScenario(r.name) === scenario);
    if (results.length === 0) continue;
    printTerminalTable(scenarioLabels[scenario] || scenario, results);
  }

  const other = headResults.filter(r => getScenario(r.name) === null);
  if (other.length > 0) {
    printTerminalTable('Other', other);
  }
  console.log();
}
