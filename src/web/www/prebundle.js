// This file is bundled into bundled.js as part of the build process.
import {NinjaKeys} from "ninja-keys"
import Worker from "web-worker"
import hotkeys from "hotkeys-js"
import Algebrite from "algebrite"
import * as Plot from "@observablehq/plot"

import {registerPatchworkViewElement} from "@inkandswitch/patchwork-elements"
import {importModuleFromFolderDocUrl} from "@inkandswitch/patchwork-filesystem"
import {registerPlugins, getRegistry} from "@inkandswitch/patchwork-plugins"

import {
	IndexedDBStorageAdapter,
	Repo,
} from "@automerge/vanillajs/slim"
import {WebSocketClientAdapter} from "@automerge/automerge-repo-network-websocket"

const repo = new Repo({
	storage: new IndexedDBStorageAdapter(),
	network: [new WebSocketClientAdapter("wss://sync3.automerge.org")],
	enableRemoteHeadsGossiping: true,
})

window.repo = repo

// Directly register the service worker (bypassing the patchwork-bootloader's
// setup() which hangs because it doesn't pass {type: "module"} and the SW
// uses ES module imports). service-worker-template.js handles its own request
// lifecycle so we don't need bootloader's handler plumbing.
await navigator.serviceWorker.register("/service-worker.js", {type: "module"})
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

// Hardcoded module URL list.
// (Previously fetched from the Tiny Patchwork module registry doc, but that
// doc requires authentication via Keyhive which Hazel's repo doesn't have.)
const moduleUrls = [
  // BAD
  // "automerge:43ry6vTh5A2doKmHgeKhyNxead61",
  // BAD
  // "automerge:36eA2C4SFzNKPnPjPAUHUpKJAAHG",
  // BAD
	// "automerge:3jBp3cskHWkTkCAMJAHfRC7mjTox",
	"automerge:3qXkpoGfWoyomfG8wTifhnzBEnpX",
	"automerge:L45rfzTVcMDsyXRyuhpNMPXqPwf",
	"automerge:2JwvUFqFGwxaZPu9CUDT5TBTq35M",
	"automerge:5sEcRSQ771Lo63SbiJuwPTDNtzX",
	"automerge:2bFZZwGF1hCN7336RuLLeMyqsH15",
	"automerge:dhkuYMpSttbRJPBJ7J5XST28bu7",
  "automerge:ovGdwzdUsBQij1yzMKp3hxATD4q",
  "automerge:b5LT7iW7jjznmRdFiPmCz2vbx7H",
  // BAD
	// "automerge:qqqwgSzRjkdf6tG3fFmMDuTZUWo",
	"automerge:3SbuS1ZvzSw1tRgXWiuhMVANysUM",
  "automerge:2RZPBJVL6nenLKR4myKz6bsHqjsz",
	"automerge:rerDfz4L6HS8hBu1BfGqAXvRViX",
  "automerge:YG3YfBzfLc9JrWFZFDheLq7aXge",
  // BAD
	// "automerge:38bVH1cbYTqv5p2BpC5drJQLLLFL",
	"automerge:4LpZgiCvBmUrqhoEK7fspCkp5jgh",
	"automerge:2c1R47RtNU6rmWMZ6LePNfC7FUe2",
  "automerge:58P7iJNhNTSSBn59Y9uLwWS6MAx",
	"automerge:Qq3G9LB5bNHwSVJ6m29Tz8zgb4E",
	"automerge:49gmqzzPJHSLc35MfCVCxWB86zo2",
	"automerge:3gH1AZwy53dHX3mqxkdfN875J3HT",
	"automerge:kFcrzeDmr5zXE1jShvxUPsoToAN",
  "automerge:fqFhWUYy3sgb3LhTDi7k5hej6PW",
  "automerge:5TNu5FZRLu3wnWaEQTL36fnXgNS",
  // BAD
	// "automerge:9FzfqZkzwhXGkj2RpfrhxrDPjPZ",
  // BAD
  // "automerge:3rxjo7nkqtnjyckeXvGDS9ELREC6",
	"automerge:3XoX2JVTrw76KGuVUAK5AXcKXf56",
	"automerge:4MCbKQoMiEnGXchaHRH3e7kxWXVs",
	"automerge:6iXwddwF9cwrjmM5yqp2xUENxUY",
	"automerge:3phkB7HzGoQ67w2ahmj9gepELErw",
  "automerge:27WwxXXqZKrjdS5weWCqiEjPenp3",
  // BAD
	// "automerge:3c61tPPwick8v75Sv9G33WjTji3R",
	"automerge:Teq1HFmGFqc37ZM7YqU6Ym7hhKJ",
	"automerge:3KLJcphTQpL3SAVhwYi2HYL5e9Fv",
	"automerge:Fhp9tNtedR7zQ2pXd9ZJBBP1XqA",
]

window.plugins = getRegistry("patchwork:tool")

// Load each module directly via the patchwork-filesystem importer.
// (We bypass ModuleWatcher because its 0.0.3 API requires a moduleSettingsUrl
// doc which is auth-gated upstream.)
async function loadModule(url) {
	try {
		const mod = await importModuleFromFolderDocUrl(url)
		console.log("Module loaded:", url, mod)
		if (Array.isArray(mod.plugins)) {
			registerPlugins(mod.plugins, url)
		}
	} catch (e) {
		console.warn(`Failed to load module ${url}:`, e)
	}
}

moduleUrls.forEach(loadModule)

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
