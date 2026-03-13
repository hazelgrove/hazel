#!/usr/bin/env node
/* Format a single benchmark JSON file as a table.
 *
 * Usage:
 *   node bench/format-table.js results.json
 *   cat results.json | node bench/format-table.js
 */

const fs = require('fs');

const input = process.argv[2]
  ? fs.readFileSync(process.argv[2], 'utf8')
  : fs.readFileSync('/dev/stdin', 'utf8');

let results;
try {
  results = JSON.parse(input);
} catch (e) {
  console.error('Error: invalid JSON input');
  process.exit(1);
}

if (!Array.isArray(results) || results.length === 0) {
  console.error('No benchmark results to display.');
  process.exit(0);
}

function formatTime(ns) {
  if (ns === undefined || ns === 0 || isNaN(ns)) return '-';
  if (ns >= 1e9) return (ns / 1e9).toFixed(2) + ' s';
  if (ns >= 1e6) return (ns / 1e6).toFixed(2) + ' ms';
  if (ns >= 1e3) return (ns / 1e3).toFixed(2) + ' us';
  return ns.toFixed(0) + ' ns';
}

function getGroup(name) {
  let count = 0;
  for (let i = 0; i < name.length; i++) {
    if (name[i] === '/') {
      count++;
      if (count === 2) return name.slice(0, i);
    }
  }
  /* Fall back to last '/' */
  const last = name.lastIndexOf('/');
  return last > 0 ? name.slice(0, last) : name;
}

const nameWidth = Math.max(10, ...results.map(r => r.name.length));
const colWidth = 12;

console.log('Benchmark'.padEnd(nameWidth) + '  ' + 'Time (median)'.padStart(colWidth));
console.log('-'.repeat(nameWidth + colWidth + 2));

let prevGroup = '';
for (const r of results) {
  const group = getGroup(r.name);
  if (group !== prevGroup) {
    if (prevGroup !== '') console.log();
    prevGroup = group;
  }
  console.log(r.name.padEnd(nameWidth) + '  ' + formatTime(r.time_ns).padStart(colWidth));
}
console.log();
