#!/usr/bin/env node

// Batch log replay script
// Usage: node scripts/batch_replay.js <input_dir> <output_dir>
//
// Reads all .json/.hzlog log files from input_dir, replays each one
// headlessly, and writes result summaries to output_dir.

const fs = require("fs");
const path = require("path");

// --- Mock browser globals before loading compiled JS ---
require("../test/headless_mocks.js");

// --- Load compiled ReasonML ---
console.log("Loading Hazel runtime...");
let hazel;
try {
  hazel = require("../_build/default/src/web/logReplay.bc.js");
} catch (e) {
  console.error(
    "Failed to load logReplay.bc.js. Make sure you've run 'dune build'."
  );
  console.error(e.message);
  process.exit(1);
}

// --- Process files ---
const args = process.argv.slice(2);
if (args.length < 2) {
  console.error(
    "Usage: node scripts/batch_replay.js <input_dir> <output_dir>"
  );
  process.exit(1);
}

const [inputDir, outputDir] = args;

if (!fs.existsSync(inputDir)) {
  console.error(`Input directory not found: ${inputDir}`);
  process.exit(1);
}

fs.mkdirSync(outputDir, { recursive: true });

const files = fs
  .readdirSync(inputDir)
  .filter((f) => f.endsWith(".json") || f.endsWith(".hzlog"));
console.log(`Found ${files.length} log files in ${inputDir}`);

let succeeded = 0;
let failed = 0;

for (const file of files) {
  const inputPath = path.join(inputDir, file);
  const baseName = file.replace(/\.(json|hzlog)$/, "");
  const outputPath = path.join(outputDir, baseName + "_result.json");

  console.log(`\nProcessing: ${file}`);

  try {
    const data = fs.readFileSync(inputPath, "utf-8");

    // Clear localStorage between runs
    globalThis.localStorage.clear();

    const resultJson = hazel.LogReplay(data);
    const result = JSON.parse(resultJson);

    fs.writeFileSync(outputPath, JSON.stringify(result, null, 2));

    if (result.error) {
      console.log(`  FAILED at action ${result.completed_actions}/${result.total_actions}: ${result.error}`);
      failed++;
    } else {
      console.log(
        `  OK: ${result.total_actions} actions, ${result.dynamics_count} evals, ` +
          `update=${result.update_time_ms.toFixed(0)}ms, calc=${result.calculate_time_ms.toFixed(0)}ms`
      );
      succeeded++;
    }
  } catch (e) {
    const errorResult = { error: `CRASHED: ${e.message}` };
    fs.writeFileSync(outputPath, JSON.stringify(errorResult, null, 2));
    console.error(`  CRASHED: ${e.message}`);
    failed++;
  }
}

console.log(`\nDone. ${succeeded} succeeded, ${failed} failed.`);
