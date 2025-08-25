#!/usr/bin/env ts-node

import { chromium } from 'playwright';
import fs from 'fs';
import yaml from 'js-yaml';
import minimist from 'minimist';

type Task = {
  url?: string;
  apiKey?: string;
  model?: string;
  initialProgram: string;
  prompt: string;
  tests: string;
};

const args = minimist(process.argv.slice(2), {
  string: ['task', 'apiKey', 'model', 'url', 'headless'],
  alias: { t: 'task' },
  default: { url: 'http://0.0.0.0:8000/', headless: 'true' },
});

function loadTask(path: string): Task {
  const doc = yaml.load(fs.readFileSync(path, 'utf8')) as Task;
  if (!doc?.initialProgram) throw new Error('YAML missing initialProgram');
  if (!doc?.prompt) throw new Error('YAML missing prompt');
  if (!doc?.tests) throw new Error('YAML missing tests');
  return doc;
}

const S = {
    modeSelect: '.mode-name select',
    assistantTab: '.assistant-button .tab',
    apiKey: '.api-input',
    apiButton: '[data-testid="update-api-key-btn"]',
    modelInput: '.llm-model-id-input',
    modelSelectButton: '[data-testid="update-model-btn"]',
    confirmAndChatButton: '[data-testid="confirm-and-chat-btn"]',
    prompt: '.message-input',
    loadingDots: '.loading-dots',
    testPass: '.test-result.Pass',
    testFail: '.test-result.Fail',
    testIndet: '.test-result.Indet',
};

// Wait until loading-dots is not in the DOM, and stays gone for debounceMs
async function waitChatDone(
  page: import('playwright').Page,
  loadingDots: string,
  debounceMs = 600,
  timeout = 30_000
) {
  const t0 = Date.now();
  while (true) {
    // Wait until no element matches (detached)
    await page.waitForSelector(loadingDots, {
      state: 'detached',
      timeout: Math.max(1, timeout - (Date.now() - t0)),
    });

    // Debounce: if it re-attaches within debounceMs, loop and wait again
    try {
      await page.waitForSelector(loadingDots, {
        state: 'attached',
        timeout: debounceMs,
      });
      continue;
    } catch {
      return;
    }
  }
}

(async () => {
  const task = loadTask(args.task);
  // CLI overrides
  const apiKey = args.apiKey ?? task.apiKey ?? '';
  const modelValue = args.model ?? task.model ?? '';
  const url = args.url ?? task.url ?? '';
  const headless = args.headless !== 'false';

  if (!apiKey) {
    console.error('Missing API key. Provide --apiKey or include in YAML.');
    process.exit(1);
  }
  if (!modelValue) {
    console.error('Missing model value. Provide --model or include in YAML.');
    process.exit(1);
  }
  if (!url) {
    console.error('Missing URL. Provide --url or include in YAML.');
    process.exit(1);
  }

  const browser = await chromium.launch({ headless });
  const ctx = await browser.newContext();
  
  // Grant clipboard permissions for all modes
  try {
    await ctx.grantPermissions(['clipboard-read', 'clipboard-write'], {
      origin: url,
    });
  } catch (e) {
    // If this fails, we'll just use the typing fallback instead of copy/paste
  }
  
  const page = await ctx.newPage();

  try {
    await page.goto(url, { waitUntil: 'domcontentloaded' });

    // Select Scratch Mode
    let select = page.locator(S.modeSelect);
    await select.waitFor({ state: 'visible' });
    await select.selectOption({ label: 'Scratch' });

    // Put initial program in the editor
    const mod = process.platform === 'darwin' ? 'Meta' : 'Control';
    await page.keyboard.press(`${mod}+A`).catch(() => {});
    await page.keyboard.press('Delete').catch(() => {});
    
    // Check if initial program needs a semicolon
    const trimmedProgram = task.initialProgram.trimEnd();
    const programToUse = trimmedProgram.endsWith(';') ? task.initialProgram : task.initialProgram + ';';
    
    // Try clipboard first, fallback to typing
    try {
      await page.evaluate(async (txt) => { 
        if (navigator.clipboard && navigator.clipboard.writeText) {
          await navigator.clipboard.writeText(txt); 
        } else {
          throw new Error('Clipboard not available');
        }
      }, programToUse);
      await page.keyboard.press(`${mod}+V`);
    } catch (e) {
      await page.keyboard.type(programToUse);
    }

    // Select Assistant tab
    await page.locator(S.assistantTab).click();

    // Fill API key
    await page.locator(S.apiKey).fill(apiKey);
    await page.locator(S.apiButton).click();

    // Need to wait for API Key to load list of models
    await page.waitForTimeout(1000);

    // Enter model ID using text input
    await page.locator(S.modelInput).fill(modelValue);
    await page.locator(S.modelSelectButton).click();

    // Open chat dialog and go to Compose mode
    await page.locator(S.confirmAndChatButton).click();
    await page.locator('.mode-buttons .mode-button', { hasText: 'Compose' }).click();

    // Wait for page change to propagate before entering prompt
    await page.waitForTimeout(1000);

    // Fill prompt
    await page.locator(S.prompt).fill(task.prompt);
    await page.keyboard.press('Enter');

    // Wait for model run to complete
    await waitChatDone(page, S.loadingDots, 600);

    // Paste tests and wait a beat for results
    await page.keyboard.press(`${mod}+A`).catch(() => {});
    await page.keyboard.press('ArrowRight');
    await page.keyboard.press('Enter');
    await page.keyboard.press('Enter');
    
    try {
      await page.evaluate(async (txt) => { 
        if (navigator.clipboard && navigator.clipboard.writeText) {
          await navigator.clipboard.writeText(txt); 
        } else {
          throw new Error('Clipboard not available');
        }
      }, task.tests);
      await page.keyboard.press(`${mod}+V`);
    } catch (e) {
      await page.keyboard.type(task.tests);
    }
    await page.waitForTimeout(500);

    // Summarize test results
    const [passCount, failCount, indetCount] = await Promise.all([
      page.locator(S.testPass).count(),
      page.locator(S.testFail).count(),
      page.locator(S.testIndet).count(),
    ]);
    const total = passCount + failCount + indetCount;

    const summary = { total, pass: passCount, fail: failCount, indet: indetCount, model: modelValue };
    console.log(JSON.stringify(summary, null, 2));

    // Optional exit code by failures:
    if (failCount > 0) process.exitCode = 2;
  } catch (e) {
    console.error(e);
    process.exitCode = 1;
  } finally {
    await browser.close();
  }
})();
