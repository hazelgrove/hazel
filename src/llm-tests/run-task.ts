#!/usr/bin/env ts-node

import { chromium } from 'playwright';
import fs from 'fs';
import yaml from 'js-yaml';
import minimist from 'minimist';
import path from 'path';

type Task = {
  url?: string;
  apiKey?: string;
  model?: string;
  initialProgram: string;
  prompt: string;
  tests: string;
};

type TokenUsageEvent = {
  prompt_tokens: number;
  completion_tokens: number;
  total_tokens: number;
  cached_tokens: number;
};

type Summary = {
  total: number;
  pass: number;
  fail: number;
  indet: number;
  model: string;
  prompt_tokens: number;
  completion_tokens: number;
  total_tokens: number;
  cached_tokens: number;
  cached_tokens_supported: boolean;
  token_events: TokenUsageEvent[];
  failed_or_indet_tests: string[];
  message_log: any[];
};

type RunOnceResult = 
    | { ok: true; summary: Summary }
    | { ok: false; error: unknown };

const args = minimist(process.argv.slice(2), {
  string: ['task', 'apiKey', 'model', 'url', 'headless', 'outputDir', 'retries', 'attemptTimeoutMs'],
  alias: { t: 'task' },
  default: { url: 'http://0.0.0.0:8000/', headless: 'true' },
});
const retries = Math.max(0, Number(args.retries ?? 2));
const attemptTimeoutMs = Math.max(1, Number(args.attemptTimeoutMs ?? 120_000));

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
  timeout = attemptTimeoutMs
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

async function runCore(
  page: import('playwright').Page,
  ctx: import('playwright').BrowserContext,
  params: {
    task: Task;
    apiKey: string;
    modelValue: string;
    url: string;
    outPath: string;
    tokenTotals: { prompt: number; completion: number; total: number; cached: number };
    tokenEvents: TokenUsageEvent[];
    cachedTokensSupported: boolean;
    lastMessages: any[] | null;
  }
): Promise<Summary> {
  const { task, apiKey, modelValue, url, outPath, tokenTotals, tokenEvents, cachedTokensSupported, lastMessages } = params;

  await page.goto(url, { waitUntil: 'domcontentloaded' });

  // Select Scratch Mode
  let select = page.locator(S.modeSelect);
  await select.waitFor({ state: 'visible' });
  await select.selectOption({ label: 'Scratch' });

  // Put initial program in the editor
  const mod = process.platform === 'darwin' ? 'Meta' : 'Control';
  await page.keyboard.press(`${mod}+A`).catch(() => {});
  await page.keyboard.press('Delete').catch(() => {});

  
  // Try clipboard first, fallback to typing
  try {
    await page.evaluate(async (txt) => { 
      if (navigator.clipboard && navigator.clipboard.writeText) {
        await navigator.clipboard.writeText(txt); 
      } else {
        throw new Error('Clipboard not available');
      }
    }, task.initialProgram);
    await page.keyboard.press(`${mod}+V`);
  } catch (e) {
    await page.keyboard.type(task.initialProgram);
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
  await page.keyboard.type(';');
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

  // Collect failed/indeterminate test contents
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
        if (navigator.clipboard && navigator.clipboard.readText) {
          return await navigator.clipboard.readText();
        }
        return '';
      });
      if (copied && copied.trim()) failed_or_indet_tests.push(copied.trim());
    }
  }
  await page.waitForTimeout(500);

  // --- Collect all messages robustly using Playwright locators ---
  const message_log: any[] = [];
  const container = await page.$('.message-display-container');
  if (container) {
    const messageEls = await container.$$('.message-container');
    for (const el of messageEls) {
      const classes = await el.getAttribute('class') || '';
      // System prompt
      if (classes.includes('system-prompt')) {
        const showBtn = await el.$('.show-prompt-button');
        let content = '[prompt hidden]';
        if (showBtn) {
          await showBtn.click();
          // Wait for the system-prompt-message to appear anywhere in the DOM
          const promptMsg = await page.waitForSelector('.system-prompt-message', { timeout: 2000 });
          content = (await promptMsg.textContent())?.trim() || '';
          // Close the prompt
          await showBtn.click();
          // Wait for the prompt to disappear before continuing
          await page.waitForSelector('.system-prompt-message', { state: 'detached', timeout: 2000 });
          await page.waitForTimeout(100);
        }
        message_log.push({ type: 'system-prompt', content });
      }
      // User message
      else if (classes.includes('user')) {
        const textarea = await el.$('textarea');
        let content = '';
        if (textarea) {
          content = (await textarea.getProperty('value')).toString() || '';
          if (!content) content = (await textarea.textContent())?.trim() || '';
        }
        message_log.push({ type: 'user', content });
      }
      // LLM/agent message
      else if (classes.includes('llm')) {
        const llmMsg = await el.$('.llm-message');
        const content = (await llmMsg?.textContent())?.trim() || '';
        message_log.push({ type: 'agent', content });
      }
      // Tool call
      else if (classes.includes('tool')) {
        const toolMsg = await el.$('.tool-message');
        const content = (await toolMsg?.textContent())?.trim() || '';
        message_log.push({ type: 'tool', content });
      }
      // System error
      else if (classes.includes('system-error')) {
        const errMsg = await el.$('.system-error-message');
        const content = (await errMsg?.textContent())?.trim() || '';
        message_log.push({ type: 'system-error', content });
      }
    }
  }
  await page.waitForTimeout(200);

  const [passCount, failCount, indetCount] = await Promise.all([
    page.locator(S.testPass).count(),
    page.locator(S.testFail).count(),
    page.locator(S.testIndet).count(),
  ]);
  const total = passCount + failCount + indetCount;

  const summary: Summary = {
    total,
    pass: passCount,
    fail: failCount,
    indet: indetCount,
    model: modelValue,
    prompt_tokens: tokenTotals.prompt,
    completion_tokens: tokenTotals.completion,
    total_tokens: tokenTotals.total || (tokenTotals.prompt + tokenTotals.completion),
    cached_tokens: tokenTotals.cached ?? 0,
    cached_tokens_supported: cachedTokensSupported,
    token_events: tokenEvents,
    failed_or_indet_tests,
    message_log: message_log
  };

  fs.writeFileSync(outPath, JSON.stringify(summary, null, 2), 'utf8');
  return summary;
}

async function runOnce(attempt: number, params: {
  headless: boolean, url: string, outDir: string, outPath: string,
  task: Task, apiKey: string, modelValue: string,
}) {
  const { headless, url, outDir, outPath, task, apiKey, modelValue } = params;

  let lastMessages: any[] | null = null;

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

  // Capture console/network for debugging
  const logs: string[] = [];
  page.on('console', m => logs.push(`[console:${m.type()}] ${m.text()}`));
  page.on('requestfailed', r => logs.push(`[requestfailed] ${r.url()} ${r.failure()?.errorText}`));

  // Start trace (stopped on success, saved on failure)
  await ctx.tracing.start({ screenshots: true, snapshots: true, sources: true });

  const attemptTag = `attempt-${attempt}`;
  const tracePath = path.join(outDir, `trace-${attemptTag}.zip`);
  const screenshotPath = path.join(outDir, `screenshot-${attemptTag}.png`);
  const logsPath = path.join(outDir, `logs-${attemptTag}.txt`);

  const work = (async () => {
    try {
      const tokenTotals = { prompt: 0, completion: 0, total: 0, cached: 0 };
      const tokenEvents: TokenUsageEvent[] = [];
      let cachedTokensSupported = false;

      // Capture OpenRouter chat completion responses
      page.on('request', async (req) => {
        try {
          if (req.method() !== 'POST') return;
          const url = req.url();
          if (!url.startsWith('https://openrouter.ai/api/v1/chat/completions')) return;
          const postData = req.postData();
          if (!postData) return;
          const body = JSON.parse(postData);
          if (Array.isArray(body.messages)) {
            lastMessages = body.messages;
          }
        } catch {}
      });
      page.on('response', async (resp) => {
        try {
          if (resp.request().method() !== 'POST') return;
          const url = resp.url();
          if (!url.startsWith('https://openrouter.ai/api/v1/chat/completions')) return;

          // Non-stream JSON responses only
          const ct = resp.headers()['content-type'] || '';
          if (!ct.includes('application/json')) return;

          const data = await resp.json();
          const u = data?.usage;
          if (!u) return;

          const p = Number(u.prompt_tokens ?? 0);
          const c = Number(u.completion_tokens ?? 0);
          const t = Number(u.total_tokens ?? (p + c));
          const cached = Number(u.prompt_tokens_details?.cached_tokens ?? 0);
          if (u.prompt_tokens_details && typeof u.prompt_tokens_details.cached_tokens !== 'undefined' && u.prompt_tokens_details.cached_tokens !== null) {
            cachedTokensSupported = true;
          }

          // Save raw event
          tokenEvents.push({
            prompt_tokens: Number.isFinite(p) ? p : 0,
            completion_tokens: Number.isFinite(c) ? c : 0,
            total_tokens: Number.isFinite(t) ? t : (Number.isFinite(p) && Number.isFinite(c) ? p + c : 0),
            cached_tokens: Number.isFinite(cached) ? cached : 0,
          });

          // Update totals
          if (Number.isFinite(p)) tokenTotals.prompt += p;
          if (Number.isFinite(c)) tokenTotals.completion += c;
          if (Number.isFinite(t)) tokenTotals.total += t;
          if (Number.isFinite(cached)) tokenTotals.cached = (tokenTotals.cached ?? 0) + cached;

          // Save last message
          if (Array.isArray(data.choices) && data.choices.length > 0 && data.choices[0].message) {
            // lastMessage = data.choices[0].message;
            // Log the full message object for debugging
            console.log(JSON.stringify(data.choices[0].message, null, 2));
          }
        } catch {
          // ignore parse/streaming issues
        }
      });
      const summary = await runCore(page, ctx, { task, apiKey, modelValue, url, outPath, tokenTotals, tokenEvents, cachedTokensSupported, lastMessages });
      // Success: stop trace without saving
      await ctx.tracing.stop();
      return { ok: true as const, summary };
    } catch (err) {
      // Failure: save trace/screenshot/logs
      try { await ctx.tracing.stop({ path: tracePath }); } catch {}
      try { await page.screenshot({ path: screenshotPath, fullPage: true }); } catch {}
      try { fs.writeFileSync(logsPath, logs.join('\n'), 'utf8'); } catch {}
      return { ok: false as const, error: err };
    } finally {
      await browser.close().catch(() => {});
    }
  })();

  // Watchdog timeout
  const timeout = new Promise<{ ok: false; error: Error }>((resolve) => {
    const t = setTimeout(() => {
      resolve({ ok: false, error: new Error(`Attempt ${attempt} timed out after ${attemptTimeoutMs}ms`) });
    }, attemptTimeoutMs);
    // If work finishes first, clear timeout
    work.finally(() => clearTimeout(t));
  });

  // Race work vs timeout
  const result = await Promise.race([work, timeout]);
  return result;
}

// 4) Retry loop with exponential backoff
(async () => {
  const outputDir = args.outputDir as string | undefined;
  if (!outputDir) {
    console.error('Missing output directory. Provide --outputDir <dir>.');
    process.exit(1);
  }
  // ensure dir exists
  fs.mkdirSync(outputDir, { recursive: true });

  const taskPath = args.task as string | undefined;
  if (!taskPath) {
    console.error('Missing task file. Provide --task <path-to-yaml>.');
    process.exit(1);
  }

  const task = loadTask(taskPath);
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

  const base = path.basename(taskPath, path.extname(taskPath));
  const outPath = path.join(outputDir, `${base}.json`);

  let lastError: unknown = null;

  for (let attempt = 1; attempt <= (retries + 1); attempt++) {
    const res = await runOnce(attempt, { headless, url, outDir: outputDir, outPath, task, apiKey, modelValue }) as RunOnceResult;
    if (res.ok) {
      console.log(JSON.stringify({ ...(res.summary ?? {}), output: outPath, attempts: attempt }, null, 2));
      if (res.summary?.fail > 0) process.exitCode = 2;
      return;
    }
    lastError = res.error;
    console.error(`[attempt ${attempt}] failed:`, res.error);

    if (attempt <= retries) {
      const backoffMs = Math.min(30_000, 2_000 * Math.pow(2, attempt - 1)); // 2s, 4s, 8s, ...
      console.error(`[attempt ${attempt}] retrying in ${backoffMs}ms...`);
      await new Promise(r => setTimeout(r, backoffMs));
    }
  }

  // All attempts failed
  console.error(`All ${retries + 1} attempts failed.`);
  if (lastError) console.error(lastError);
  process.exit(1);
})();