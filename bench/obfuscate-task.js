#!/usr/bin/env node
// Rename every binding in a Hazel task program to a meaningless name and drop
// comments, so a navigation run cannot lean on names to find its targets.
//
//   node bench/obfuscate-task.js bench/tasks/nav-parts.hz > bench/tasks/nav-parts-obf.hz
//
// Prints the name mapping to stderr; nav_targets for the -obf task are the
// source task's paths pushed through it. Only let/module/type binders are
// renamed (in order of first appearance, so the output is deterministic);
// lambda parameters and builtins keep their names. Re-run `hazel run` on the
// output: the value must equal the source task's.

const fs = require("fs");

const obfuscate = (source) => {
  const withoutComments = source
    .replace(/^[ \t]*#[^#\n]*#[ \t]*\n/gm, "")
    .replace(/[ \t]*#[^#\n]*#/g, "")
    .replace(/\n{3,}/g, "\n\n");
  const binders = [...withoutComments.matchAll(/\b(let|module|type)\s+([A-Za-z_][A-Za-z0-9_]*)/g)];
  const mapping = new Map();
  const counters = { let: 0, module: 0, type: 0 };
  const prefix = { let: "v", module: "M", type: "T" };
  for (const [, kind, name] of binders) {
    if (mapping.has(name)) continue;
    counters[kind] += 1;
    mapping.set(name, prefix[kind] + String(counters[kind]).padStart(2, "0"));
  }
  // One pass over identifiers, so a new name can never be renamed again.
  const renamed = withoutComments.replace(/\b[A-Za-z_][A-Za-z0-9_]*\b/g, (id) => mapping.get(id) ?? id);
  return { renamed, mapping };
};

const { renamed, mapping } = obfuscate(fs.readFileSync(process.argv[2], "utf8"));
process.stdout.write(renamed.trimStart());
for (const [from, to] of mapping) process.stderr.write(`${from} -> ${to}\n`);
