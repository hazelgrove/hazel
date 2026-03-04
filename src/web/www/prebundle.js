// This file is bundled into bundle.js as part of the build process.
import { NinjaKeys } from "ninja-keys";
import Worker from 'web-worker';
import hotkeys from "hotkeys-js";
import Algebrite from "algebrite";
import * as Plot from "@observablehq/plot";

import {registerPatchworkViewElement} from "@inkandswitch/patchwork-elements"
import {
	ModuleWatcher,
	createFilesystemHandoffHandler,
} from "@inkandswitch/patchwork-filesystem"
import setup from "@inkandswitch/patchwork-bootloader"
import {registerPlugins, getRegistry} from "@inkandswitch/patchwork-plugins"

import {
	IndexedDBStorageAdapter,
	Repo,
	WebSocketClientAdapter,
} from "@automerge/vanillajs/slim"

const repo = new Repo({
	network: [new WebSocketClientAdapter("wss://sync3.automerge.org")],
	storage: new IndexedDBStorageAdapter(),
})

window.repo = repo

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

const handoff = createFilesystemHandoffHandler(repo)
setup(async (href, request) => handoff(href, request))

const moduleWatcher = new ModuleWatcher(
	repo,
	// rabbitcounter tool + datatype
  // ["automerge:4GHeCq7k1BEhPdpPDWyF2EQFXKrG"],
  // codemirror-base
  ["automerge:3qXkpoGfWoyomfG8wTifhnzBEnpX"],
  // ["automerge:9omX3o8Ch387BWzEKW2KXfySPxe"],
	(name, mod) => {
		console.log("Prebundled module loaded:", name, mod)
		if (Array.isArray(mod.plugins)) {
			// TODO: maybe get rid of this check?
			registerPlugins(mod.plugins, name)
		}
	}
)
window.moduleWatcher = moduleWatcher
console.log("moduleWatcher", moduleWatcher)
window.plugins = getRegistry("patchwork:tool")

// "automerge:2kbRgPyThsqsAtFsaEV83AuQtHsr"

// subsequent dynamic loading of modules
moduleWatcher.loadModules([
  "automerge:3qXkpoGfWoyomfG8wTifhnzBEnpX",
  "automerge:L45rfzTVcMDsyXRyuhpNMPXqPwf",
  "automerge:3Fj5zE7QdhbbWVJNsAHPbf84YfX6",
  // petrinaut tool?
  "automerge:3phkB7HzGoQ67w2ahmj9gepELErw",
  "automerge:Qq3G9LB5bNHwSVJ6m29Tz8zgb4E"
])

registerPatchworkViewElement({repo, moduleWatcher})

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
