#!/usr/bin/env node
/* Compare two benchmark JSON files and print a table.
 *
 * Usage:
 *   node bench/compare.js base.json head.json
 *
 * Also used by the GitHub Actions perf workflow. */

const fs = require('fs');

const [,, baseFile, headFile] = process.argv;

if (!baseFile || !headFile) {
  console.error('Usage: node bench/compare.js <base.json> <head.json>');
  process.exit(1);
}

let baseResults = [];
try {
  baseResults = JSON.parse(fs.readFileSync(baseFile, 'utf8'));
} catch (e) {
  console.error(`Warning: could not read base results (${e.message})`);
}
const headResults = JSON.parse(fs.readFileSync(headFile, 'utf8'));

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
  const emoji = pct > 10 ? ' ⚠️' : pct < -10 ? ' 🚀' : '';
  return `${sign}${pct.toFixed(1)}%${emoji}`;
}

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

const editResults = headResults.filter(r => r.name.includes('/edit/'));
const memoResults = headResults.filter(r => r.name.includes('/memo/'));

printTable('Edit Cycle (per-keystroke latency)', editResults);
printTable('Memo-hit overhead (repeated calls, same input)', memoResults);
console.log();
