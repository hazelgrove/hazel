// extractInBrowser.d.ts
interface ExtractResult {
	text: string;
	mapping: Array<{
		index: number;
		selectorPath: string;
	}>;
}

declare function extractInBrowser(selector: string): ExtractResult;

export = extractInBrowser;
