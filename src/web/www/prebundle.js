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

const handoff = createFilesystemHandoffHandler(repo)
setup(async (href, request) => handoff(href, request))

const moduleWatcher = new ModuleWatcher(
	repo,
	// rabbitcounter
	["automerge:4GHeCq7k1BEhPdpPDWyF2EQFXKrG"],
	(name, mod) => {
		console.log("Prebundled module loaded:", name, mod)
		if (Array.isArray(mod.plugins)) {
			// TODO: maybe get rid of this check?
			registerPlugins(mod.plugins, name)
		}
	}
)

// "automerge:2kbRgPyThsqsAtFsaEV83AuQtHsr"

//moduleWatcher.loadModules(["automerge:4GHeCq7k1BEhPdpPDWyF2EQFXKrG"])

registerPatchworkViewElement({repo})

window.Algebrite = Algebrite
window.Plot = Plot

// This is the default behavior for the hotkeys module but I'm overriding it for the
// clipboard-shim and the ninja-keys command palette (which lives inside a shadow DOM).
hotkeys.filter = event => {
	// composedPath() lets us see the original target even when the event has been
	// retargeted across a shadow DOM boundary (e.g. the <input> inside ninja-keys).
	const path = typeof event.composedPath === 'function' ? event.composedPath() : []
	const target = event.target || event.srcElement
	const {tagName, id} = target

	// Override happening here
	if (id == "clipboard-shim") {
		return true
	}

	// When the event originates inside the ninja-keys command palette, only let
	// its own navigation/close keys through. This stops globally-registered action
	// hotkeys (e.g. Cmd+A for "Select All") from firing while the user is typing
	// in the palette's search box, while still letting Esc close the palette and
	// the arrow/enter keys navigate it.
	const inNinjaKeys = path.some(el => el && el.tagName === 'NINJA-KEYS')
	if (inNinjaKeys) {
		return ['Escape', 'Enter', 'ArrowUp', 'ArrowDown', 'Backspace', 'Tab'].includes(event.key)
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
