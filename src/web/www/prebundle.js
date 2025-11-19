// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from "ninja-keys"
import hotkeys from "hotkeys-js"
import Algebrite from "algebrite"
import * as Plot from "@observablehq/plot"
import bootstrapPatchwork from "./setup-service-worker.js"
import {registerPatchworkViewElement} from "@patchwork/elements"
import {ModuleWatcher} from "@patchwork/filesystem"
import {registerPlugins} from "@patchwork/plugins"

const {repo} = await bootstrapPatchwork()
window.repo = repo

const moduleWatcher = new ModuleWatcher(
	"automerge:4GHeCq7k1BEhPdpPDWyF2EQFXKrG",
	["automerge:BDesnARCFFupAzaSQ9nhnTYHE3B"],
	repo,
	(name, mod) => {
		if (Array.isArray(mod.plugins)) {
			// TODO: maybe get rid of this check?
			registerPlugins(mod.plugins, name)
		}
	}
)

registerPatchworkViewElement({
	repo,
	moduleWatcher: /** @type {ModuleWatcher} */ (/** @type {unknown} */ (null)),
})

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
