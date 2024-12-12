import puppeteer, { Page, ElementHandle, Browser } from "puppeteer";
import { PuppeteerScreenRecorder } from "puppeteer-screen-recorder";
import {
	emulateKeyCombination,
	findSpanByQuery,
	findSpanByRowCol,
} from "./utils";

process.chdir('../out');

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
	aspectRatio: "1:1",
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

	async typeString(string: string, delay = 0, waitForCompletion = false) {
		// select the code container
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

	async clickRowCol(row: number, col: number) {
		const codeContainer = await this.elementHandle.$(
			".code-container .code-text",
		);
		if (!codeContainer) throw new Error("Code container not found");

		const html = await codeContainer.evaluate((el) => el.innerHTML);
		console.log("html:", html);
		const span = findSpanByRowCol(html, row, col);

		if (span) {
			await span.click();
		} else {
			throw new Error(`Span not found at row ${row} and col ${col}`);
		}
	}

	async cursorToEnd() {
		// press right arrow key until cursor reaches end
		const codeContainer = await this.elementHandle.$(
			".code-container .code-text",
		);
		if (!codeContainer) throw new Error("Code container not found");

		for (let i = 0; i < 50; i++) {
			await codeContainer.press("ArrowRight");
		}
	}

	async clickQuery(query: string) {
		const codeContainer = await this.elementHandle.$(
			".code-container .code-text",
		);
		if (!codeContainer) throw new Error("Code container not found");

		const html = await codeContainer.evaluate((el) => el.innerHTML);
		console.log("html:", html);
		const span = findSpanByQuery(html, query);

		if (span) {
			await span.click();
		} else {
			throw new Error(`Span not found for query "${query}"`);
		}
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

//const livelit = `(^popup("<marquee>the haters are ill-prepared</marquee>"))`;
const livelit = `
	let emotion = (^emotion(50)) in
	case (emotion) 
	| "happy" => "Hooray! What a pleasant day!"
	| "neutral" => "Things are medium, I suppose."
	| "sad" => "Sorrow sorrow, today and tomorrow."
	| _ => "You have broken my trust, and frankly, our friendship"
	end
`;
// Usage example
(async () => {
	const controller = new HazelController();
	await controller.launch("http://localhost:8000", true);

	// Select "Exercises" mode
	await controller.selectMode("Scratch");

	// Get all cells
	const cells = await controller.getCells();

	// edit the first cell with id "YourImpl"
	const scratchCell = cells[0];

	await scratchCell.clearContent();
	await scratchCell.typeString(livelit, 25);
	await wait(250);
	// go to the end line
	await scratchCell.cursorToEnd();
	await wait(250);

	// projector "livelit"
	await controller.setProjectorMode("livelit");
	await wait(500);
	// await controller.enableProjector();
	// await wait(1000);

	// save screenshot
	await controller.page?.screenshot({ path: `screenshot-${new Date().getTime()}.png` });

	// go to popup
	const pages = await controller.browser!.pages(); // get all open pages by the browser
	const popup = pages[pages.length - 1];
	await popup.bringToFront();

	const zoomCommand = `document.body.style.fontSize = "72px";`;
	await popup.evaluate(zoomCommand);
	await wait(5000);
	// Close browser
	await controller.close();
})();
