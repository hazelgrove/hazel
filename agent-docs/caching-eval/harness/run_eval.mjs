#!/usr/bin/env node
/**
 * Prompt-caching eval harness.
 *
 * Validates, per model, whether OpenRouter prompt caching actually works by
 * mirroring the app's production request shape (see src/util/OpenRouter.re):
 *   - endpoint: https://openrouter.ai/api/v1/chat/completions
 *   - cache breakpoint: multipart content block with cache_control on the
 *     stable system prefix (the app's "floor" anchor)
 *   - top-level session_id for sticky provider routing
 *
 * Protocol per model (3 turns, same session):
 *   turn 1: system(anchored) + user            -> expect cache WRITE (~prefix)
 *   turn 2: + assistant + user                 -> expect cache READ  (~prefix)
 *   turn 3: + assistant + user                 -> expect cache READ  (~prefix, flat)
 *
 * Verdict: WORKING if turn>=2 cache_read >= 50% of the turn-1 write (or of the
 * prefix estimate when the provider caches implicitly and reports no write),
 * AND billed cost(turn 2) < cost(turn 1). Otherwise NOT_WORKING; missing
 * usage data => INCONCLUSIVE.
 *
 * Data accumulators (append-only; nothing is overwritten across runs):
 *   outputs/raw/<runid>.requests.jsonl   every request body as sent
 *   outputs/raw/<runid>.responses.jsonl  every raw response JSON as received
 *   outputs/summary/results.jsonl        one structured row per (run, model, turn)
 *   outputs/summary/verdicts.jsonl       one verdict row per (run, model)
 *   outputs/summary/ledger.jsonl         /credits before+after each run (spend cross-check)
 *   outputs/summary/results.csv          flat CSV of all results.jsonl rows (regenerated)
 *
 * Usage:
 *   OPENROUTER_API_KEY=... node run_eval.mjs --dry-run   # free: validate models, print exact cost estimate
 *   OPENROUTER_API_KEY=... node run_eval.mjs             # paid: run the eval
 *   options: --models a,b,c   override matrix ids
 *            --turns N        override turns per model
 *            --anchor-history also anchor the last history message (phase-2 probe)
 */

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const HERE = path.dirname(fileURLToPath(import.meta.url));
const OUT = path.join(HERE, "..", "outputs");
const API = "https://openrouter.ai/api/v1";
const CHARS_PER_TOKEN = 4; // conservative prose heuristic, used only for sizing the prefix

// ---------- config ----------

const cfg = JSON.parse(fs.readFileSync(path.join(HERE, "models.json"), "utf8"));
const argv = process.argv.slice(2);
const flag = (name) => argv.includes(name);
const opt = (name) => {
  const i = argv.indexOf(name);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : null;
};

const DRY_RUN = flag("--dry-run");
const ANCHOR_HISTORY = flag("--anchor-history");
const TURNS = Number(opt("--turns")) || cfg.protocol.turns_per_model;
const MODEL_IDS = opt("--models")
  ? opt("--models").split(",")
  : cfg.models.map((m) => m.id);

const KEY = process.env.OPENROUTER_API_KEY;
if (!KEY) {
  console.error("OPENROUTER_API_KEY is not set.");
  process.exit(1);
}

// ---------- deterministic stable prefix ----------

/** Deterministic filler well above every provider's min-cacheable threshold
 *  (Anthropic: 1024–2048 tokens depending on model). Numbered lines make any
 *  accidental truncation visible in logs. */
function buildPrefix(targetTokens) {
  const line = (i) =>
    `[${String(i).padStart(4, "0")}] The Hazel caching eval measures whether a long, ` +
    `byte-stable system prefix is served from the provider prompt cache on repeat ` +
    `turns; this line is deterministic filler and carries no instruction.\n`;
  let text =
    "You are a terse assistant used by an automated caching benchmark. " +
    "Follow the user's instruction exactly and reply with as few tokens as possible.\n\n";
  for (let i = 0; text.length / CHARS_PER_TOKEN < targetTokens; i++) text += line(i);
  return text;
}

const PREFIX = buildPrefix(cfg.protocol.prefix_target_tokens);
const PREFIX_EST_TOKENS = Math.round(PREFIX.length / CHARS_PER_TOKEN);

/** Two inert tool definitions so the payload shape (tools present) matches the
 *  app; tool_choice stays default and max_tokens is tiny, so they are never called. */
const TOOLS = [
  {
    type: "function",
    function: {
      name: "noop_probe",
      description: "Inert probe tool. Never call it.",
      parameters: { type: "object", properties: {}, additionalProperties: false },
    },
  },
  {
    type: "function",
    function: {
      name: "noop_probe_two",
      description: "Second inert probe tool. Never call it.",
      parameters: { type: "object", properties: {}, additionalProperties: false },
    },
  },
];

const USER_TURNS = [
  "Reply with exactly: OK-1",
  "Reply with exactly: OK-2",
  "Reply with exactly: OK-3",
  "Reply with exactly: OK-4",
  "Reply with exactly: OK-5",
];

// ---------- wire shapes (mirror OpenRouter.re) ----------

const anchored = (text) => [
  { type: "text", text, cache_control: { type: "ephemeral" } },
];

function buildMessages(history, userText) {
  const msgs = [{ role: "system", content: anchored(PREFIX) }, ...history];
  if (ANCHOR_HISTORY && msgs.length > 1) {
    const last = msgs[msgs.length - 1];
    msgs[msgs.length - 1] = { ...last, content: anchored(String(last.content)) };
  }
  msgs.push({ role: "user", content: userText });
  return msgs;
}

function buildPayload(modelId, sessionId, messages) {
  return {
    model: modelId,
    temperature: 0,
    top_p: 1.0,
    tools: TOOLS,
    stream: false,
    max_tokens: cfg.protocol.max_completion_tokens,
    session_id: sessionId,
    messages,
  };
}

// ---------- accumulators ----------

const RUN_ID = new Date().toISOString().replace(/[:.]/g, "-");
fs.mkdirSync(path.join(OUT, "raw"), { recursive: true });
fs.mkdirSync(path.join(OUT, "summary"), { recursive: true });

const appendJsonl = (file, obj) =>
  fs.appendFileSync(file, JSON.stringify(obj) + "\n");
const rawReq = path.join(OUT, "raw", `${RUN_ID}.requests.jsonl`);
const rawRes = path.join(OUT, "raw", `${RUN_ID}.responses.jsonl`);
const resultsFile = path.join(OUT, "summary", "results.jsonl");
const verdictsFile = path.join(OUT, "summary", "verdicts.jsonl");
const ledgerFile = path.join(OUT, "summary", "ledger.jsonl");

function regenerateCsv() {
  const rows = fs
    .readFileSync(resultsFile, "utf8")
    .trim()
    .split("\n")
    .filter(Boolean)
    .map((l) => JSON.parse(l));
  const cols = [
    "run_id", "model", "turn", "prompt_tokens", "completion_tokens",
    "cache_read_tokens", "cache_write_tokens", "cost_credits", "cache_discount",
    "prefix_est_tokens", "anchor_history", "latency_ms",
  ];
  const csv = [
    cols.join(","),
    ...rows.map((r) => cols.map((c) => r[c] ?? "").join(",")),
  ].join("\n");
  fs.writeFileSync(path.join(OUT, "summary", "results.csv"), csv + "\n");
}

// ---------- API helpers ----------

async function api(pathname, init = {}) {
  const res = await fetch(`${API}${pathname}`, {
    ...init,
    headers: {
      Authorization: `Bearer ${KEY}`,
      "Content-Type": "application/json",
      ...init.headers,
    },
  });
  const body = await res.json().catch(() => ({}));
  if (!res.ok) {
    throw new Error(`${pathname} -> HTTP ${res.status}: ${JSON.stringify(body).slice(0, 400)}`);
  }
  return body;
}

const getCredits = async () => (await api("/credits")).data;
const getModels = async () => (await api("/models")).data;

/** Normalized usage extraction. Field locations per OpenRouter's usage
 *  accounting docs (validated 2026-08-04): cache metrics live in
 *  prompt_tokens_details (cached_tokens = reads, cache_write_tokens = writes,
 *  explicit-caching models only); cost is the amount actually charged, in
 *  credits, and is always included — no request flag needed. cache_discount
 *  is OpenRouter's own per-response statement of the saving. Anthropic-native
 *  cache_creation_input_tokens kept as fallback (mirrors OpenRouter.re). */
function extractUsage(responseJson) {
  const u = responseJson.usage ?? {};
  const details = u.prompt_tokens_details ?? {};
  return {
    prompt_tokens: u.prompt_tokens ?? null,
    completion_tokens: u.completion_tokens ?? null,
    cache_read_tokens: details.cached_tokens ?? u.cache_read_input_tokens ?? null,
    cache_write_tokens:
      details.cache_write_tokens ??
      u.cache_write_tokens ??
      u.cache_creation_input_tokens ??
      null,
    cost_credits: u.cost ?? null,
    cache_discount: u.cache_discount ?? null,
  };
}

// ---------- estimation (used by --dry-run and the pre-flight print) ----------

function estimate(models) {
  const perModel = models.map((m) => {
    const pIn = parseFloat(m.pricing?.prompt ?? "0");
    const pOut = parseFloat(m.pricing?.completion ?? "0");
    const pWrite = parseFloat(m.pricing?.input_cache_write ?? "0") || pIn * 1.25;
    const pRead = parseFloat(m.pricing?.input_cache_read ?? "0") || pIn * 0.1;
    let cost = 0;
    for (let t = 0; t < TURNS; t++) {
      const overhead = 60 + t * 40; // user turns + short assistant history
      const inTok = PREFIX_EST_TOKENS + overhead;
      cost +=
        t === 0
          ? PREFIX_EST_TOKENS * pWrite + overhead * pIn
          : PREFIX_EST_TOKENS * pRead + overhead * pIn;
      cost += cfg.protocol.max_completion_tokens * pOut;
      void inTok;
    }
    // worst case if caching is entirely broken: everything at full input rate
    let worst = 0;
    for (let t = 0; t < TURNS; t++)
      worst +=
        (PREFIX_EST_TOKENS + 60 + t * 40) * pIn +
        cfg.protocol.max_completion_tokens * pOut;
    return { id: m.id, expected_usd: cost, worst_case_usd: worst };
  });
  const total = perModel.reduce((s, m) => s + m.expected_usd, 0);
  const worst = perModel.reduce((s, m) => s + m.worst_case_usd, 0);
  return { perModel, total, worst, llm_calls: models.length * TURNS };
}

// ---------- verdicts ----------

function verdictFor(turnRows) {
  const t1 = turnRows[0];
  const later = turnRows.slice(1);
  if (!t1 || later.length === 0) return { verdict: "INCONCLUSIVE", reason: "not enough turns" };
  if (later.every((r) => r.cache_read_tokens == null))
    return { verdict: "INCONCLUSIVE", reason: "no cache_read reported on any later turn" };
  const baseline = t1.cache_write_tokens || PREFIX_EST_TOKENS;
  const reads = later.map((r) => r.cache_read_tokens ?? 0);
  const readOk = reads.some((r) => r >= 0.5 * baseline);
  const costOk =
    t1.cost_credits != null && later[0].cost_credits != null
      ? later[0].cost_credits < t1.cost_credits
      : null;
  if (readOk && costOk !== false)
    return {
      verdict: "WORKING",
      reason: `cache_read ${Math.max(...reads)} vs baseline ${baseline}` +
        (costOk ? `; cost dropped ${t1.cost_credits} -> ${later[0].cost_credits}` : ""),
    };
  return {
    verdict: "NOT_WORKING",
    reason: `cache_read ${JSON.stringify(reads)} vs baseline ${baseline}; ` +
      `cost t1=${t1.cost_credits} t2=${later[0]?.cost_credits}`,
  };
}

// ---------- main ----------

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function runModel(modelId) {
  const sessionId = `caching-eval-${RUN_ID}-${modelId.replace(/\//g, "_")}`;
  const history = [];
  const turnRows = [];
  for (let t = 0; t < TURNS; t++) {
    const userText = USER_TURNS[t % USER_TURNS.length];
    const messages = buildMessages(history, userText);
    const payload = buildPayload(modelId, sessionId, messages);
    appendJsonl(rawReq, { run_id: RUN_ID, model: modelId, turn: t + 1, payload });
    const started = Date.now();
    let response, error = null;
    try {
      response = await api("/chat/completions", {
        method: "POST",
        body: JSON.stringify(payload),
      });
    } catch (e) {
      error = String(e);
    }
    const latency_ms = Date.now() - started;
    appendJsonl(rawRes, { run_id: RUN_ID, model: modelId, turn: t + 1, latency_ms, error, response });
    if (error) {
      console.error(`  turn ${t + 1}: ERROR ${error}`);
      turnRows.push(null);
      break;
    }
    const usage = extractUsage(response);
    const row = {
      run_id: RUN_ID,
      model: modelId,
      turn: t + 1,
      ...usage,
      prefix_est_tokens: PREFIX_EST_TOKENS,
      anchor_history: ANCHOR_HISTORY,
      latency_ms,
    };
    appendJsonl(resultsFile, row);
    turnRows.push(row);
    console.log(
      `  turn ${t + 1}: in=${usage.prompt_tokens} out=${usage.completion_tokens} ` +
      `cache_read=${usage.cache_read_tokens} cache_write=${usage.cache_write_tokens} ` +
      `cost=${usage.cost_credits}`
    );
    const reply = response.choices?.[0]?.message?.content ?? "";
    history.push({ role: "user", content: userText });
    history.push({ role: "assistant", content: reply });
    if (t + 1 < TURNS) await sleep(cfg.protocol.inter_turn_delay_ms);
  }
  const v = verdictFor(turnRows.filter(Boolean));
  const verdictRow = { run_id: RUN_ID, model: modelId, ...v, turns: turnRows.filter(Boolean).length };
  appendJsonl(verdictsFile, verdictRow);
  console.log(`  verdict: ${v.verdict} (${v.reason})`);
  return verdictRow;
}

async function main() {
  console.log(`run ${RUN_ID}`);
  console.log(`prefix ~${PREFIX_EST_TOKENS} est. tokens, ${TURNS} turns/model, anchor_history=${ANCHOR_HISTORY}`);

  const live = await getModels();
  const byId = new Map(live.map((m) => [m.id, m]));
  const selected = [];
  for (const id of MODEL_IDS) {
    if (byId.has(id)) selected.push(byId.get(id));
    else console.error(`model NOT FOUND on OpenRouter, skipping: ${id}`);
  }
  if (selected.length === 0) {
    console.error("no valid models; aborting.");
    process.exit(1);
  }

  const est = estimate(selected);
  console.log(`\nplanned LLM calls: ${est.llm_calls}`);
  for (const m of est.perModel)
    console.log(`  ${m.id}: expected ~$${m.expected_usd.toFixed(4)} (worst case $${m.worst_case_usd.toFixed(4)})`);
  console.log(`total expected ~$${est.total.toFixed(4)} (worst case $${est.worst.toFixed(4)})\n`);

  if (DRY_RUN) {
    fs.writeFileSync(
      path.join(OUT, "summary", "estimate.json"),
      JSON.stringify({ run_id: RUN_ID, prefix_est_tokens: PREFIX_EST_TOKENS, turns: TURNS, ...est }, null, 2) + "\n"
    );
    console.log("dry run: no LLM calls made. Estimate written to outputs/summary/estimate.json");
    return;
  }

  const before = await getCredits();
  appendJsonl(ledgerFile, { run_id: RUN_ID, phase: "before", credits: before });

  const verdicts = [];
  for (const m of selected) {
    console.log(`\n=== ${m.id} ===`);
    verdicts.push(await runModel(m.id));
  }

  const after = await getCredits();
  appendJsonl(ledgerFile, { run_id: RUN_ID, phase: "after", credits: after });
  const spent = (after?.total_usage ?? 0) - (before?.total_usage ?? 0);

  regenerateCsv();

  console.log("\n=== summary ===");
  for (const v of verdicts) console.log(`${v.verdict.padEnd(12)} ${v.model}  (${v.reason})`);
  console.log(`\ncredits ledger delta: $${spent.toFixed(4)} (cross-check vs summed per-request cost)`);
  console.log(`raw data: outputs/raw/${RUN_ID}.*.jsonl`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
