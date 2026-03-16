// This file is bundled into bundled.js as part of the build process.
import {NinjaKeys} from "ninja-keys"
import Worker from "web-worker"
import hotkeys from "hotkeys-js"
import Algebrite from "algebrite"
import * as Plot from "@observablehq/plot"

import {registerPatchworkViewElement} from "@inkandswitch/patchwork-elements"
import {ModuleWatcher} from "@inkandswitch/patchwork-filesystem"
import setup from "@inkandswitch/patchwork-bootloader"
import {registerPlugins, getRegistry} from "@inkandswitch/patchwork-plugins"

import {
	IndexedDBStorageAdapter,
	MessageChannelNetworkAdapter,
	Repo,
} from "@automerge/vanillajs/slim"

const repo = new Repo({
	storage: new IndexedDBStorageAdapter(),
	async sharePolicy(peerId) {
		return peerId.includes("service-worker")
	},
	enableRemoteHeadsGossiping: true,
})

window.repo = repo

const result = await setup()
if (!result) {
	throw new Error("Failed to set up service worker")
}

repo.networkSubsystem.addNetworkAdapter(
	new MessageChannelNetworkAdapter(result.port)
)
await repo.networkSubsystem.whenReady()

window.getRepoChannel = () => {
	const {port1, port2} = new MessageChannel()
	navigator.serviceWorker.controller.postMessage({type: "port"}, [port2])
	return port1
}

window.hazelWriteToDoc = async (docUrl, jsonString) => {
	try {
		const handle = await repo.find(docUrl)
		const parsed = JSON.parse(jsonString)

		handle.change(doc => {
			// Only sync top-level keys in doc.store (where tldraw keeps shapes)
			if (parsed.store && doc.store) {
				// Delete keys in doc.store that aren't in parsed.store
				for (const key of Object.keys(doc.store)) {
					if (!(key in parsed.store)) {
						delete doc.store[key]
					}
				}
				// Add/update keys from parsed.store
				for (const [key, value] of Object.entries(parsed.store)) {
					doc.store[key] = value
				}
			}
		})
	} catch (e) {
		console.error("hazelWriteToDoc error:", e)
	}
}

const moduleWatcher = new ModuleWatcher(
	repo,
	// codemirror-base
	["automerge:3qXkpoGfWoyomfG8wTifhnzBEnpX"],
	(name, mod) => {
		console.log("Prebundled module loaded:", name, mod)
		if (Array.isArray(mod.plugins)) {
			registerPlugins(mod.plugins, name)
		}
	}
)
window.moduleWatcher = moduleWatcher
console.log("moduleWatcher", moduleWatcher)
window.plugins = getRegistry("patchwork:tool")

// subsequent dynamic loading of modules
moduleWatcher.loadModules([
	"automerge:3qXkpoGfWoyomfG8wTifhnzBEnpX",
	"automerge:L45rfzTVcMDsyXRyuhpNMPXqPwf",
	"automerge:3Fj5zE7QdhbbWVJNsAHPbf84YfX6",
	// petrinaut tool?
	"automerge:3phkB7HzGoQ67w2ahmj9gepELErw",
	"automerge:Qq3G9LB5bNHwSVJ6m29Tz8zgb4E",
])

registerPatchworkViewElement({repo})

window.Algebrite = Algebrite
window.Plot = Plot

// This is the default behavior for the hotkeys module but I'm overriding it for the clipboard-shim
hotkeys.filter = event => {
	const target = event.target || event.srcElement
	const {tagName, id} = target

	// Override happening here
	if (id == "clipboard-shim") {
		return true
	}

	let flag = true
	const isInput =
		tagName === "INPUT" &&
		![
			"checkbox",
			"radio",
			"range",
			"button",
			"file",
			"reset",
			"submit",
			"color",
		].includes(target.type)
	// ignore: isContentEditable === 'true', <input> and <textarea> when readOnly state is false, <select>
	if (
		target.isContentEditable ||
		((isInput || tagName === "TEXTAREA" || tagName === "SELECT") &&
			!target.readOnly)
	) {
		flag = false
	}
	return flag
}
