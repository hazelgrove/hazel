import puppeteer, { Page, ElementHandle, Browser } from "puppeteer";
import { PuppeteerScreenRecorder } from "puppeteer-screen-recorder";
import {
	emulateKeyCombination,
    extractTextAndMapping
} from "./utils";

process.chdir('./out');

const RecorderConfig = {
	followNewTab: true,
	fps: 60,
	videoFrame: {
		width: 1024,
		height: 512,
	},
	videoCrf: 18,
	videoCodec: "libx264",
	videoPreset: "ultrafast",
	videoBitrate: 1000,
	autopad: {
		color: "black",
	},
	aspectRatio: "2:1",
};

function wait(time: number) {
	return new Promise(function (resolve) {
		setTimeout(resolve, time);
	});
}

class Cell {
	private page: Page;
	private elementHandle: ElementHandle;
	readonly classes: string[];
	readonly ids: string[];

	constructor(
		page: Page,
		elementHandle: ElementHandle,
		classes: string[],
		ids: string[] = [],
	) {
		this.page = page;
		this.elementHandle = elementHandle;
		this.classes = classes;
		this.ids = ids;
	}

	async getContent(): Promise<string | null> {
		return await this.page.evaluate(
			(el) => el.textContent,
			this.elementHandle,
		);
	}

	async scroll() {
		await wait(100);
		await this.page.evaluate(
			(el) =>
				el.children[0].scrollIntoView({
					block: "center",
					behavior: "smooth",
				}),
			this.elementHandle,
		);
		await wait(1000);
	}

	async backspaceContent(n = 1, delay = 0) {
		const contentHandle = await this.elementHandle.$(
			".code-container .code-text",
		);
		if (contentHandle) {
			await contentHandle.click();
			for (let i = 0; i < n; i++) {
				await contentHandle.press("Backspace");
				await setTimeout(() => {}, delay);
			}
		}
	}

	async clearContent() {
		const contentHandle = await this.elementHandle.$(
			".code-container .code-text",
		);
		if (contentHandle) {
			await emulateKeyCombination(
				this.page,
				contentHandle,
				"a",
				"Control",
			);
			await contentHandle.press("Backspace");
		}
	}

	async typeString(string: string, delay = 0) {
		const codeContainer = await this.elementHandle.$(
			".code-container .code-text",
		);
		if (!codeContainer) throw new Error("Code container not found");
		await codeContainer.click();

		const lines = string.split("\n");
		for (let i = 0; i < lines.length; i++) {
			for (let j = 0; j < lines[i].length; j++) {
				await codeContainer.type(lines[i][j]);
				await wait(delay);
			}
			if (i !== lines.length - 1) {
				await this.page.keyboard.press("Enter");
			}
		}
	}

	async cursorToEnd() {
		const codeContainer = await this.elementHandle.$(
			".code-container .code-text",
		);
		if (!codeContainer) throw new Error("Code container not found");

		// Press the right arrow key multiple times to move cursor to end.
		for (let i = 0; i < 50; i++) {
			await codeContainer.press("ArrowRight");
		}
	}

	/**
	 * Searches the cell's code container text for a given substring and clicks on the position
	 * at the start or end of that substring. Uses extractTextAndMapping to map from text indices
	 * to elements, then clicks the corresponding element. This won't necessarily guarantee cursor
	 * placement at a precise character boundary, but it should get you close.
	 */
	async clickAtSubstring(substring: string, position: "start" | "end" = "end") {
		const containerSelector = ".code-container .code-text";
		const { text, mapping } = await extractTextAndMapping(this.page, containerSelector);

		const index = text.indexOf(substring);
		if (index === -1) {
			throw new Error(`Substring "${substring}" not found`);
		}
		const charIndex = position === "start" ? index : index + substring.length - 1;
		const target = mapping.find((m) => m.index === charIndex);
		if (!target) {
			throw new Error(`Mapping not found for character index ${charIndex}`);
		}

		const elementHandle = await this.page.$(target.selectorPath);
		if (!elementHandle) {
			throw new Error(`Element at selector path ${target.selectorPath} not found`);
		}

        console.log(`Clicking at character ${charIndex} in substring "${substring}"`);

		// Click the element that represents that character position
		await elementHandle.click();
	}
}

class HazelController {
	browser: Browser | null = null;
	private recorder: PuppeteerScreenRecorder | null = null;
	page: Page | null = null;

	async launch(url: string, record = false) {
		this.browser = await puppeteer.launch({ headless: false });
		this.page =
			(await this.browser.pages().then((pages) => pages[0])) ||
			(await this.browser.newPage());
		await this.page.setViewport({ width: 1024, height: 512 });
		await this.page.goto(url);

		await this.page?.waitForFunction(() => {
			const loading = document.querySelector(".loading");
			return !loading;
		});

		if (record) {
			this.recorder = new PuppeteerScreenRecorder(
				this.page,
				RecorderConfig,
			);
			await this.recorder.start(`demo-${Date.now()}.mp4`);
		}
	}

	async close() {
		if (this.browser) {
			await this.browser.close();
			await this.recorder?.stop();
		}
	}

	async selectMode(mode: string) {
		await this.page?.select("#editor-mode select", mode);

		await this.page?.waitForFunction(
			(mode) => {
				const main = document.getElementById("main");
				return main && main.classList.contains(mode);
			},
			{},
			mode,
		);
	}

	async setProjectorMode(mode: string) {
		console.log("Setting projector mode to", mode);
		await this.page?.select("#projectors select", mode);
	}

	async enableProjector() {
		console.log("Enabling projector");
		const toggleSwitch = await this.page?.$(".toggle-knob");
		if (toggleSwitch) {
            console.log("Clicking toggle switch", toggleSwitch);
			await toggleSwitch.click();
		} else {
			throw new Error("Toggle switch not found");
		}
	}

	async getCells(): Promise<Cell[]> {
		const cells = await this.page?.$$(".cell");
		if (!cells) return [];

		return Promise.all(
			cells.map(async (elementHandle, index) => {
				let classes = await elementHandle.evaluate(
					(el) => el.children[0].className,
				);
				let ids = [];
				if (classes.includes("cell-item")) {
					const childrenClasses = await elementHandle.evaluate((el) =>
						Array.from(el.children[0].children)
							.map((child: Element) => child.className)
							.join(" "),
					);
					classes += " " + childrenClasses;
				}
				ids = await elementHandle.evaluate((el) =>
					Array.from(el.querySelectorAll("[id]")).map(
						(el: Element) => el.id,
					),
				);
				return new Cell(
					this.page!,
					elementHandle,
					classes.split(" "),
					ids,
				);
			}),
		);
	}

	async waitForCompletion() {
		throw new Error("Not implemented");
	}
}

// const livelit = `
// 	let emotion = (^emotion(50)) in
// 	case (emotion) 
// 	| "happy" => "Hooray! What a pleasant day!"
// 	| "neutral" => "Things are medium, I suppose."
// 	| "sad" => "Sorrow sorrow, today and tomorrow."
// 	| _ => "You have broken my trust, and frankly, our friendship"
// 	end
// `;

const invocation = `(^slider(50))`;

const livelit = `
  let slider = ${invocation} in
  case (slider)
    | n => "Slider value: " ++ string_of_int(n)
  end
`;

(async () => {
	const controller = new HazelController();
	try {
		await controller.launch("http://localhost:8000", true);

		// Select "Exercises" mode
		await controller.selectMode("Scratch");

		// Get all cells
		const cells = await controller.getCells();

		const scratchCell = cells[0];
		await scratchCell.clearContent();
		await scratchCell.typeString(livelit, 25);
		await wait(250);
		await scratchCell.cursorToEnd();
		await wait(250);

		// Click at the end of (^emotion(50))
		await scratchCell.clickAtSubstring(invocation, "end");
		await wait(250);

		// projector "livelit"
		await controller.setProjectorMode("livelit");
		await wait(500);

		// save screenshot
		await controller.page?.screenshot({ path: `screenshot-${Date.now()}.png` });

		// // Go to popup
		// const pages = await controller.browser!.pages(); 
		// const popup = pages[pages.length - 1];
		// await popup.bringToFront();

		// const zoomCommand = `document.body.style.fontSize = "72px";`;
		// await popup.evaluate(zoomCommand);
		await wait(1000);

        // refresh page
        await controller.page?.reload();

        // wait for page to load
        await wait(5000);

		// Close browser normally if all is successful
		await controller.close();
	} catch (error) {
		console.error('An error occurred:', error);
		// Do not close the browser here, so it remains open for debugging.
	}
})();