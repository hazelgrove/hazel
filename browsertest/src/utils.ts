import { Page, ElementHandle } from "puppeteer";

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

import { JSDOM } from "jsdom";

interface SpanInfo {
	element: HTMLElement;
	startOffset: number;
	endOffset: number;
}

export function parseCodeString(codeString: string): {
	text: string;
	spans: SpanInfo[];
} {
	const dom = new JSDOM(codeString, { url: "http://localhost" });
	const body = dom.window.document.body;

	const spans: SpanInfo[] = [];
	let text = "";
	let offset = 0;

	function processNode(node: Node) {
		if (node.nodeType === dom.window.Node.TEXT_NODE) {
			const nodeText = node.textContent || "";
			if (!/^\s*$/.test(nodeText)) {
				// Only add text nodes that are not pure whitespace
				text += nodeText;
				offset += nodeText.length;
			}
		} else if (node.nodeType === dom.window.Node.ELEMENT_NODE) {
			const element = node as HTMLElement;
			if (element.classList.contains("linebreak")) {
				text += "\n";
				offset += 1;
			} else if (element.classList.contains("whitespace")) {
				// Add the whitespace character(s)
				const whitespaceText = element.textContent || " ";
				text += whitespaceText;
				offset += whitespaceText.length;
				// Optionally, do not add whitespace elements to the spans
				// If you prefer to track them, you can include the following lines:
				// const startOffset = offset - whitespaceText.length;
				// const endOffset = offset;
				// spans.push({ element: element, startOffset, endOffset });
			} else {
				const startOffset = offset;
				for (let i = 0; i < element.childNodes.length; i++) {
					processNode(element.childNodes[i]);
				}
				const endOffset = offset;
				spans.push({ element: element, startOffset, endOffset });
			}
		}
	}

	for (let i = 0; i < body.childNodes.length; i++) {
		processNode(body.childNodes[i]);
	}

	return { text, spans };
}

export function findSpanByRowCol(
	codeString: string,
	row: number,
	col: number,
): HTMLElement | null {
	const { text, spans } = parseCodeString(codeString);

	// Convert row and col to character offset
	const lines = text.split("\n");
	if (row < 1 || row > lines.length) {
		return null; // Row out of range
	}
	const line = lines[row - 1];
	if (col < 1 || col > line.length + 1) {
		return null; // Column out of range
	}

	let charOffset = 0;
	for (let i = 0; i < row - 1; i++) {
		charOffset += lines[i].length + 1; // +1 for the newline character
	}
	charOffset += col - 1;

	// Find the span that covers this offset
	for (const spanInfo of spans) {
		if (
			charOffset >= spanInfo.startOffset &&
			charOffset < spanInfo.endOffset
		) {
			return spanInfo.element;
		}
	}

	return null;
}

export function findSpanByQuery(
	codeString: string,
	query: string,
): HTMLElement | null {
	const { text, spans } = parseCodeString(codeString);

	const index = text.indexOf(query);
	if (index === -1) {
		return null; // Query not found
	}

	// Find the first span that covers any character in the query
	for (const spanInfo of spans) {
		if (
			(index >= spanInfo.startOffset && index < spanInfo.endOffset) ||
			(spanInfo.startOffset > index &&
				spanInfo.startOffset < index + query.length)
		) {
			return spanInfo.element;
		}
	}

	return null;
}
