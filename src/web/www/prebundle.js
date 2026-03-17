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
import {WebSocketClientAdapter} from "@automerge/automerge-repo-network-websocket"

const repo = new Repo({
	storage: new IndexedDBStorageAdapter(),
	network: [new WebSocketClientAdapter("wss://sync3.automerge.org")],
	enableRemoteHeadsGossiping: true,
})

window.repo = repo

const result = await setup()
if (result?.port) {
	repo.networkSubsystem.addNetworkAdapter(
		new MessageChannelNetworkAdapter(result.port)
	)
}
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
			if (!parsed || typeof parsed !== "object") return
			// Sync all top-level keys from parsed into doc
			for (const key of Object.keys(doc)) {
				if (!(key in parsed)) {
					delete doc[key]
				}
			}
			for (const [key, value] of Object.entries(parsed)) {
				doc[key] = value
			}
		})
	} catch (e) {
		console.error("hazelWriteToDoc error:", e)
	}
}

// Fetch module URLs from the Tiny Patchwork module registry.
// Falls back to known modules if the registry doc hasn't synced yet.
const fallbackModules = [
	"automerge:3qXkpoGfWoyomfG8wTifhnzBEnpX",
	"automerge:L45rfzTVcMDsyXRyuhpNMPXqPwf",
	"automerge:3Fj5zE7QdhbbWVJNsAHPbf84YfX6",
	"automerge:3phkB7HzGoQ67w2ahmj9gepELErw",
	"automerge:Qq3G9LB5bNHwSVJ6m29Tz8zgb4E",
]

let moduleUrls = fallbackModules
try {
	const registryUrl = "automerge:2LZBb891v37vggWYQPJRbYdyBGGE"
	const handle = await repo.find(registryUrl)
	const doc = handle.doc()
	if (doc && Array.isArray(doc.modules) && doc.modules.length > 0) {
		console.log("Loaded module URLs from registry:", doc.modules)
		moduleUrls = doc.modules
	}
} catch (e) {
	console.warn("Failed to read module registry, using fallback:", e)
}

const moduleWatcher = new ModuleWatcher(
	repo,
	moduleUrls,
	(name, mod) => {
		console.log("Module loaded:", name, mod)
		if (Array.isArray(mod.plugins)) {
			registerPlugins(mod.plugins, name)
		}
	}
)
window.moduleWatcher = moduleWatcher
console.log("moduleWatcher", moduleWatcher)
window.plugins = getRegistry("patchwork:tool")

moduleWatcher.loadModules(moduleUrls)

registerPatchworkViewElement({repo})

// Prevent tldraw popover focus from scrolling #main.
// When Radix UI opens a popover (e.g. the "More" tools menu), it focuses the
// popover content, which causes the browser to scroll the nearest scrollable
// ancestor (#main) to bring it into view. We save and restore the scroll
// position to counteract this.
document.addEventListener("focusin", (e) => {
	if (e.target && e.target.closest && e.target.closest("patchwork-view")) {
		const main = document.getElementById("main")
		if (main) {
			const top = main.scrollTop
			const left = main.scrollLeft
			requestAnimationFrame(() => {
				if (main.scrollTop !== top || main.scrollLeft !== left) {
					main.scrollTop = top
					main.scrollLeft = left
				}
			})
		}
	}
}, true)

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
