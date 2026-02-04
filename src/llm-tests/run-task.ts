#!/usr/bin/env ts-node

import { chromium, Page, Locator } from 'playwright';
import fs from 'fs';
import yaml from 'js-yaml';
import minimist from 'minimist';
import path from 'path';

/* ----------------------------- Types ----------------------------- */

type Task = {
  url?: string;
  apiKey?: string;
  model?: string;
  initialProgram: string;
  prompt: string;
  tests: string;
};

type Summary = {
  model: string;
  total: number;
  pass: number;
  fail: number;
  indet: number;
  failed_or_indet_tests: string[];
  tokens: {
    prompt: number;
    completion: number;
    total: number;
    cached: number;
    cached_supported: boolean;
  };
  messages: any[];
};

type RunOnceResult =
  | { ok: true; summary: Summary }
  | { ok: false; error: unknown };

/* ----------------------------- CLI ----------------------------- */

const args = minimist(process.argv.slice(2), {
  string: ['task', 'apiKey', 'model', 'url', 'headless', 'outputDir', 'retries', 'attemptTimeoutMs'],
  alias: { t: 'task' },
  default: { url: 'http://0.0.0.0:8000/', headless: 'true' },
});

const retries = Math.max(0, Number(args.retries ?? 2));
const attemptTimeoutMs = Math.max(1, Number(args.attemptTimeoutMs ?? 120_000));

function loadTask(p: string): Task {
  const doc = yaml.load(fs.readFileSync(p, 'utf8')) as Task;
  if (!doc?.initialProgram) throw new Error('YAML missing initialProgram');
  if (!doc?.prompt) throw new Error('YAML missing prompt');
  if (!doc?.tests) throw new Error('YAML missing tests');
  return doc;
}

const S = {
  modeSelect: '.mode-name select',
  assistantTab: '.assistant-button .tab',
  apiKey: '.agent-api-key-input',
  apiButton: '[data-testid="update-api-key-btn"]',
  confirmAndChatButton: '.confirm-settings-button',
  prompt: '.chat-message-input',
  testPass: '.test-result.Pass',
  testFail: '.test-result.Fail',
  testIndet: '.test-result.Indet',
};

async function pasteOrTypeText(
  page: Page,
  text: string,
  mod: 'Control' | 'Meta'
) {
  try {
    await page.evaluate(async (txt) => {
      if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(txt);
      } else {
        throw new Error('Clipboard not available');
      }
    }, text);

    await page.keyboard.press(`${mod}+V`);
  } catch {
    await page.keyboard.type(text);
  }
}

async function retryFillUntil(
  locator: Locator,
  value: string,
  timeoutMs = 5000
) {
  const start = Date.now();

  while (Date.now() - start < timeoutMs) {
    await locator.fill(value);

    if (await locator.inputValue() === value) {
      return;
    }

    await locator.page().waitForTimeout(100);
  }

  throw new Error(`Failed to fill input within ${timeoutMs}ms`);
}

async function waitOpenRouterChatDone(
  page: import('playwright').Page,
  prompt: string,
  opts?: {
    idleMs?: number;
    timeoutMs?: number;
  }
) {
  const idleMs = opts?.idleMs ?? 1000;
  const timeoutMs = opts?.timeoutMs ?? 120_000;

  let inflight = 0;
  let lastZero = Date.now();

  const onRequest = (req: any) => {
    if (
      req.method() === 'POST' &&
      req.url().startsWith('https://openrouter.ai/api/v1/chat/completions')
    ) {
      inflight++;
    }
  };

  const onFinished = (req: any) => {
    if (
      req.method() === 'POST' &&
      req.url().startsWith('https://openrouter.ai/api/v1/chat/completions')
    ) {
      inflight = Math.max(0, inflight - 1);
      if (inflight === 0) lastZero = Date.now();
    }
  };

  page.on('request', onRequest);
  page.on('requestfinished', onFinished);
  page.on('requestfailed', onFinished);

  await page.locator(S.prompt).fill(prompt);
  await page.keyboard.press('Enter');

  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    if (inflight === 0 && Date.now() - lastZero >= idleMs) {
      cleanup();
      return;
    }
    await page.waitForTimeout(100);
  }

  cleanup();
  throw new Error('Timed out waiting for OpenRouter chat completion');
  
  function cleanup() {
    page.off('request', onRequest);
    page.off('requestfinished', onFinished);
    page.off('requestfailed', onFinished);
  }
}

async function runCore(
  page: Page,
  params: {
    task: Task;
    apiKey: string;
    modelValue: string;
    url: string;
    outPath: string;
  }
): Promise<Summary> {
  const { task, apiKey, modelValue, url, outPath } = params;

  page.on('dialog', async d => d.accept('foo'));

  await page.goto(url, { waitUntil: 'domcontentloaded' });

  await page.locator(S.modeSelect).waitFor({ state: 'visible' });
  await page.locator(S.modeSelect).selectOption({ label: 'Projects' });

  await page.getByTitle('Folder actions').hover();
  await page.getByTitle('Add file').click();
  await page.waitForTimeout(500);

  await page.locator('.code-container > .code').first().click({ force: true });

  const mod = process.platform === 'darwin' ? 'Meta' : 'Control';
  await page.keyboard.press(`${mod}+A`).catch(() => {});
  await page.keyboard.press('Delete').catch(() => {});

  await pasteOrTypeText(page, task.initialProgram, mod);

  await page.locator(S.assistantTab).click();

  await retryFillUntil(page.locator(S.apiKey), apiKey);

  await page.locator(S.apiButton).first().click({ force: true });

  await page.waitForTimeout(1000);

  await page
    .locator('.llm-id')
    .getByText(modelValue, { exact: true })
    .locator('..')
    .click();
  await page.locator(S.confirmAndChatButton).click();

  await page.waitForTimeout(500);

  await waitOpenRouterChatDone(page, task.prompt, { timeoutMs: attemptTimeoutMs });

  await page.locator('.code-container > .code').first().click({ force: true });
  await page.keyboard.press(`${mod}+A`).catch(() => {});
  await page.keyboard.press('ArrowRight');
  await page.keyboard.press('Enter');
  await page.keyboard.type(';\n');
  await pasteOrTypeText(page, task.tests, mod);

  await page.waitForTimeout(500);

  /* ---------- Collect failed / indeterminate tests ---------- */

  const failed_or_indet_tests: string[] = [];

  for (const selector of [S.testFail, S.testIndet]) {
    const tests = page.locator(selector);
    const count = await tests.count();
    for (let i = 0; i < count; i++) {
      const test = tests.nth(i);
      await test.click({ clickCount: 3, force: true });
      await page.waitForTimeout(100); // let selection happen
      await page.keyboard.press(`${mod}+c`);
      await page.waitForTimeout(100); // let clipboard update
      // Read clipboard in browser context
      const copied = await page.evaluate(async () => {
        const selection = window.getSelection();
        return selection ? selection.toString() : '';
      });
      if (copied && copied.trim()) failed_or_indet_tests.push(copied.trim());
    }
  }

  /* ---------- Export Messages JSON ---------- */

  const downloadPromise = page.waitForEvent('download');
  await page.getByTitle('Export Messages (JSON)').click();
  const download = await downloadPromise;

  const tmpMessagesPath = outPath + '.messages.tmp.json';
  await download.saveAs(tmpMessagesPath);
  const messages = JSON.parse(fs.readFileSync(tmpMessagesPath, 'utf8'));
  fs.unlinkSync(tmpMessagesPath);

  /* ---------- Test counts ---------- */

  const [pass, fail, indet] = await Promise.all([
    page.locator(S.testPass).count(),
    page.locator(S.testFail).count(),
    page.locator(S.testIndet).count(),
  ]);

  const summary: Summary = {
    model: modelValue,
    total: pass + fail + indet,
    pass,
    fail,
    indet,
    failed_or_indet_tests,
    tokens: {
      prompt: 0,
      completion: 0,
      total: 0,
      cached: 0,
      cached_supported: false,
    },
    messages,
  };

  fs.writeFileSync(outPath, JSON.stringify(summary, null, 2), 'utf8');
  return summary;
}

async function runOnce(attempt: number, params: {
  headless: boolean;
  url: string;
  outDir: string;
  outPath: string;
  task: Task;
  apiKey: string;
  modelValue: string;
}) {
  const browser = await chromium.launch({ headless: params.headless, slowMo: 50 });
  const ctx = await browser.newContext();

  try {
    await ctx.grantPermissions(['clipboard-read', 'clipboard-write'], {
      origin: params.url,
    });
  } catch {}

  const page = await ctx.newPage();

  try {
    const summary = await runCore(page, params);
    await browser.close();
    return { ok: true as const, summary };
  } catch (error) {
    await browser.close();
    return { ok: false as const, error };
  }
}

/* ----------------------------- Retry Loop ----------------------------- */

(async () => {
  if (!args.outputDir || !args.task) {
    console.error('Usage: --task <yaml> --outputDir <dir>');
    process.exit(1);
  }

  fs.mkdirSync(args.outputDir, { recursive: true });

  const task = loadTask(args.task);
  const apiKey = args.apiKey ?? task.apiKey ?? '';
  const modelValue = args.model ?? task.model ?? '';
  const url = args.url ?? task.url ?? '';
  const headless = args.headless !== 'false';

  const base = path.basename(args.task, path.extname(args.task));
  // Add timestamp to filename to avoid collisions
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const outPath = path.join(args.outputDir, `${base}_${timestamp}.json`);

  let lastError: unknown = null;

  for (let attempt = 1; attempt <= retries + 1; attempt++) {
    const res = await runOnce(attempt, {
      headless,
      url,
      outDir: args.outputDir,
      outPath,
      task,
      apiKey,
      modelValue,
    }) as RunOnceResult;

    if (res.ok) {
      console.log(JSON.stringify({ output: outPath, attempts: attempt }, null, 2));
      if (res.summary.fail > 0) process.exitCode = 2;
      return;
    }

    lastError = res.error;
    console.error(`[attempt ${attempt}] failed`, res.error);
    if (attempt <= retries) await new Promise(r => setTimeout(r, 2000 * attempt));
  }

  console.error('All attempts failed');
  console.error(lastError);
  process.exit(1);
})();
