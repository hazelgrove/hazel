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

/* Diverging bar chart using log scale.
 * Log scale makes ratios symmetric: 2x faster and 2x slower are same bar length.
 * Uses Unicode block characters for smooth sub-cell rendering. */
const ANSI = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  dim: '\x1b[2m',
  reset: '\x1b[0m',
};

// Block characters from 1/8 to full block (for fractional fill)
const BLOCKS = [' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█'];

function makeBar(ratio, maxCells) {
  if (!ratio || !isFinite(ratio) || ratio === 1) return '';
  // log2 scale: each cell = one doubling/halving
  const logVal = Math.log2(ratio);
  const clamped = Math.max(-maxCells, Math.min(maxCells, logVal));
  const abs = Math.abs(clamped);
  const fullCells = Math.floor(abs);
  const frac = abs - fullCells;
  const fracChar = BLOCKS[Math.round(frac * 8)];
  const color = logVal > 0 ? ANSI.red : ANSI.green;

  if (logVal > 0) {
    // Slower: bar goes right from center
    return color + '█'.repeat(fullCells) + fracChar + ANSI.reset;
  } else {
    // Faster: bar goes left from center
    const bar = fracChar + '█'.repeat(fullCells);
    // Right-align the bar so it touches the center
    return color + bar.padStart(maxCells) + ANSI.reset;
  }
}

function printGraph(title, results) {
  const comparable = results.filter(r => {
    const b = baseMap[r.name];
    return b && b > 0 && !isNaN(b) && r.time_ns > 0 && !isNaN(r.time_ns);
  });
  if (comparable.length === 0) return;

  const maxCells = 6; // each cell = 2x, so ±6 cells covers up to 64x
  const nameWidth = Math.max(10, ...comparable.map(r => r.name.length));

  console.log(`\n${title}`);

  // Scale line: position labels at their log2 positions
  //   -6    -3    0    +3    +6
  //   64x   8x    │    8x    64x
  const scaleChars = Array(maxCells * 2 + 1).fill(' ');
  scaleChars[maxCells] = '│';
  const scaleLine = ' '.repeat(nameWidth) + '  ' +
    ANSI.green + '← faster' + ANSI.reset +
    ' '.repeat(Math.max(0, maxCells * 2 + 1 - 16)) +
    ANSI.red + 'slower →' + ANSI.reset;
  console.log(scaleLine);

  // Tick marks
  const ticks = Array(maxCells * 2 + 1).fill('─');
  ticks[maxCells] = '┼';
  ticks[0] = '├'; ticks[maxCells * 2] = '┤';
  ticks[maxCells - 3] = '┼'; ticks[maxCells + 3] = '┼';
  console.log(
    ' '.repeat(nameWidth) + '  ' +
    ANSI.dim + ticks.join('') + ANSI.reset
  );

  for (const r of comparable) {
    const base = baseMap[r.name];
    const ratio = r.time_ns / base;
    const bar = makeBar(ratio, maxCells);

    const ratioStr = ratio >= 1
      ? `${ratio.toFixed(1)}x`
      : `${(1/ratio).toFixed(1)}x`;

    if (ratio >= 1) {
      // Slower: bar goes right from center
      const leftPad = ' '.repeat(maxCells);
      console.log(
        r.name.padEnd(nameWidth) + '  ' +
        leftPad + '│' + bar + ' ' + ratioStr
      );
    } else {
      // Faster: bar goes left from center
      console.log(
        r.name.padEnd(nameWidth) + '  ' +
        bar + '│' + ' ' + ratioStr
      );
    }
  }
}

const editResults = headResults.filter(r => r.name.includes('/edit/'));
const memoResults = headResults.filter(r => r.name.includes('/memo/'));

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
    console.log(makeTable(editResults));

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

  printTable('Edit Cycle (per-keystroke latency)', editResults);
  printTable('Memo-hit overhead (repeated calls, same input)', memoResults);

  printGraph('Edit Cycle', editResults);
  printGraph('Memo-hit', memoResults);
  console.log();
}
