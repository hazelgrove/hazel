#!/usr/bin/env node

import {build} from "esbuild"
import {wasmLoader} from "esbuild-plugin-wasm"
import {fileURLToPath} from "node:url"
import path from "node:path"

// Packages resolved at runtime from gaios.sgai.uk (page: via the importmap in
// index.html; service worker: via swExternalMap below). Mirrors
// @inkandswitch/patchwork-bootloader@0.6.2's externals-list; the locally
// installed bootloader (0.0.4) predates the subduction/providers packages.
const externals = [
	"@automerge/automerge",
	"@automerge/automerge/slim",
	"@automerge/automerge-repo",
	"@automerge/automerge-repo/slim",
	"@automerge/automerge-repo/worker-port",
	"@automerge/automerge-repo/subduction-websocket-worker-shared",
	"@automerge/automerge-repo-network-messagechannel",
	"@automerge/automerge-repo-network-websocket",
	"@automerge/automerge-repo-storage-indexeddb",
	"@automerge/automerge-repo-keyhive",
	"@automerge/automerge-subduction",
	"@automerge/automerge-subduction/slim",
	"@keyhive/keyhive",
	"@keyhive/keyhive/slim",
	"@inkandswitch/patchwork-bootloader",
	"@inkandswitch/patchwork-elements",
	"@inkandswitch/patchwork-filesystem",
	"@inkandswitch/patchwork-plugins",
	"@inkandswitch/patchwork-providers",
	"@inkandswitch/patchwork",
	"@codemirror/state",
	"@codemirror/view",
	"@codemirror/language",
	"@codemirror/commands",
	"solid-js",
	"solid-js/html",
	"solid-js/web",
	"solid-js/h",
	"solid-js/store",
	"solid-js/jsx-runtime",
]

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const entryPoint = path.join(__dirname, "prebundle.js")
const outFile = path.join(__dirname, "bundled.js")

const swEntryPoint = path.join(__dirname, "service-worker-template.js")
const swOutFile = path.join(__dirname, "service-worker.js")

// Service worker externals resolve to gaios URLs (SW can't use importmaps)
const swExternalMap = Object.fromEntries(
	externals.map(name => [name, `https://gaios.sgai.uk/packages/${name}.js`])
)

const swExternalPlugin = {
	name: "sw-externals",
	setup(build) {
		const externalNames = new Set(externals)
		build.onResolve({filter: /.*/}, args => {
			if (externalNames.has(args.path)) {
				return {path: swExternalMap[args.path], external: true}
			}
		})
	},
}

async function run() {
	await Promise.all([
		build({
			entryPoints: [entryPoint],
			bundle: true,
			outfile: outFile,
			absWorkingDir: __dirname,
			format: "esm",
			platform: "browser",
			target: "esnext",
			logLevel: "info",
			plugins: [wasmLoader()],
			external: externals,
		}),
		build({
			entryPoints: [swEntryPoint],
			bundle: true,
			outfile: swOutFile,
			absWorkingDir: __dirname,
			format: "esm",
			platform: "browser",
			target: "esnext",
			logLevel: "info",
			plugins: [swExternalPlugin],
		}),
	])
}

run().catch(error => {
	console.error(error)
	process.exit(1)
})
