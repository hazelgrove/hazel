#!/usr/bin/env node

// Bundles prebundle.js into bundled.js. Replaces a CLI esbuild invocation
// because we need the wasm-loader plugin to handle Automerge's WASM
// imports; the plugin embeds the WASM as base64, so bundled.js is the only
// output and no separate .wasm file needs to be served.

import {build} from "esbuild";
import {wasmLoader} from "esbuild-plugin-wasm";
import path from "node:path";
import {fileURLToPath} from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

await build({
  entryPoints: [path.join(__dirname, "prebundle.js")],
  outfile: path.join(__dirname, "bundled.js"),
  absWorkingDir: __dirname,
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "esnext",
  logLevel: "info",
  plugins: [wasmLoader({mode: "embedded"})],
});
