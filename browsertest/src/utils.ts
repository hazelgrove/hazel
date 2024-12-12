import { Page, ElementHandle } from "puppeteer";
import extractInBrowser from "./extract";

/**
 * Emulates a key combination (e.g., Ctrl+A, Ctrl+C) by dispatching custom keyboard
 * events on the given element. Events are dispatched to ensure that any `Event#preventDefault`
 * which would have normally occurred in the application as a result of the key combination
 * is respected.
 *
 * @param {Page} page - Puppeteer page object.
 * @param {ElementHandle} elm - The element to send the key combination to.
 * @param {string} key - The key to press (e.g., 'a' for Ctrl+A).
 * @param {string} modifierKey - The modifier key (e.g., 'Control' or 'Meta').
 * @return {Promise<void>} Promise resolving once the key emulation completes.
 */
export async function emulateKeyCombination(
	page: Page,
	elm: ElementHandle,
	key: string,
	modifierKey?: "Control" | "Meta",
): Promise<void> {
	await page.evaluate(
		(elm, key, modifierKey) => {
			const isMac = /Mac|iPod|iPhone|iPad/.test(
				window.navigator.platform,
			);
			const modifier = isMac ? "Meta" : "Control";
			const activeElement = elm as HTMLElement;

			activeElement.dispatchEvent(
				new KeyboardEvent("keydown", {
					bubbles: true,
					cancelable: true,
					key: modifier,
					code: modifier === "Meta" ? "MetaLeft" : "ControlLeft",
					location: window.KeyboardEvent.DOM_KEY_LOCATION_LEFT,
					ctrlKey: modifier === "Control",
					metaKey: modifier === "Meta",
					charCode: 0,
					keyCode: modifier === "Meta" ? 93 : 17,
					which: modifier === "Meta" ? 93 : 17,
				}),
			);

			const preventableEvent = new KeyboardEvent("keydown", {
				bubbles: true,
				cancelable: true,
				key,
				code: `Key${key.toUpperCase()}`,
				location: window.KeyboardEvent.DOM_KEY_LOCATION_STANDARD,
				ctrlKey: modifier === "Control",
				metaKey: modifier === "Meta",
				charCode: 0,
				keyCode: key.toUpperCase().charCodeAt(0),
				which: key.toUpperCase().charCodeAt(0),
			});

			const wasPrevented =
				!activeElement.dispatchEvent(preventableEvent) ||
				preventableEvent.defaultPrevented;

			if (!wasPrevented && key === "a") {
				// If 'a' was pressed and not prevented, we trigger select all
				document.execCommand("selectall", false);
			}

			activeElement.dispatchEvent(
				new KeyboardEvent("keyup", {
					bubbles: true,
					cancelable: true,
					key: modifier,
					code: modifier === "Meta" ? "MetaLeft" : "ControlLeft",
					location: window.KeyboardEvent.DOM_KEY_LOCATION_LEFT,
					charCode: 0,
					keyCode: modifier === "Meta" ? 93 : 17,
					which: modifier === "Meta" ? 93 : 17,
				}),
			);
		},
		elm,
		key,
		modifierKey,
	);
}

export async function extractTextAndMapping(page: Page, parentSelector: string) {
	const { text, mapping } = await page.evaluate(extractInBrowser, parentSelector);
	return { text, mapping };
}