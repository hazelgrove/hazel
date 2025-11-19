#!/usr/bin/env node

import {build} from "esbuild"
import {wasmLoader} from "esbuild-plugin-wasm"
import {fileURLToPath} from "node:url"
import path from "node:path"

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const entryPoint = path.join(__dirname, "prebundle.js")
const outFile = path.join(__dirname, "bundled.js")

async function run() {
	await build({
		entryPoints: [entryPoint],
		bundle: true,
		outfile: outFile,
		absWorkingDir: __dirname,
		format: "esm",
		platform: "browser",
		target: "esnext",
		logLevel: "info",
		plugins: [wasmLoader()],
		external: ["@automerge/*", "@patchwork/*"],
	})
}

run().catch(error => {
	console.error(error)
	process.exit(1)
})
