// Runs agentfill_bench.bc.js with OpenRouter behind `benchAsk`.
//
//   OPENROUTER_KEY_FILE=<file with OPENROUTER_API_KEY=...> \
//   node --stack-size=8192 --require ./test/idb_stub.js bench/agentfill/run.js \
//     --model google/gemini-3-flash-preview --intervention none \
//     --out results.json [--only a,b] [--max-calls 60] [--check-examples]
//
// The key is read from the file and sent only in the request header.
const fs = require("fs");
const path = require("path");

const args = process.argv.slice(2);
const opt = (name, dflt) => {
  const i = args.indexOf("--" + name);
  return i < 0 ? dflt : args[i + 1];
};
const flag = (name) => args.includes("--" + name);
const model = opt("model", "google/gemini-3-flash-preview");
const out = opt("out", "results.json");
const maxCalls = parseInt(opt("max-calls", "60"));
const only = opt("only", null);
// Caps the rounds per theorem below the product's own limit, to bound cost.
const rounds = parseInt(opt("rounds", "1000"));
// OpenRouter reasoning effort (low, medium, high), or none.
const reasoning = opt("reasoning", null);

const keyLine = fs
  .readFileSync(process.env.OPENROUTER_KEY_FILE, "utf8")
  .split("\n")
  .find((l) => l.startsWith("OPENROUTER_API_KEY="));
const key = keyLine.slice("OPENROUTER_API_KEY=".length).trim().replace(/^["']|["']$/g, "");

const items = JSON.parse(fs.readFileSync(path.join(__dirname, "benchmark.json"), "utf8"));
globalThis.benchConfig = JSON.stringify({
  items,
  intervention: opt("intervention", "none"),
  only: only ? only.split(",") : null,
  check_examples: flag("check-examples"),
  // "tools": the chat agent's way, with the proof tools; "loop": the fill loop.
  mode: opt("mode", "loop"),
  turns: parseInt(opt("rounds", "12")),
});

let current = null;
let calls = 0;
let itemCalls = 0;
const exchanges = [];
let cost = 0;
globalThis.benchProgress = (name) => {
  current = name;
  itemCalls = 0;
  process.stderr.write(`[bench] ${name}\n`);
};
globalThis.benchAsk = (messagesJson, k) => {
  const messages = JSON.parse(messagesJson);
  if (calls >= maxCalls) {
    k(false, "call budget spent");
    return;
  }
  if (itemCalls >= rounds) {
    k(false, `stopped after ${rounds} rounds`);
    return;
  }
  itemCalls++;
  if (model === "reference") {
    const item = items.find((i) => i.name === current);
    const m = item.reference && item.reference.match(new RegExp(`theorem ${current} = [\\s\\S]*? proof ([\\s\\S]*) in\\n0$`));
    const reply = m ? m[1] : "?";
    exchanges.push({ item: current, messages, reply, cost: 0 });
    k(true, reply);
    return;
  }
  calls++;
  fetch("https://openrouter.ai/api/v1/chat/completions", {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: "Bearer " + key },
    body: JSON.stringify({
      model,
      messages,
      temperature: 1.0,
      usage: { include: true },
      ...(reasoning ? { reasoning: { effort: reasoning } } : {}),
    }),
  })
    .then((r) => r.json())
    .then((j) => {
      if (j.error) {
        exchanges.push({ item: current, messages, error: j.error.message });
        k(false, `request failed: ${j.error.message}`);
        return;
      }
      const reply = j.choices[0].message.content || "";
      const c = (j.usage && j.usage.cost) || 0;
      cost += c;
      const thinking = j.choices[0].message.reasoning || null;
      exchanges.push({ item: current, messages, reply, thinking, cost: c, usage: j.usage });
      k(true, reply);
    })
    .catch((e) => {
      exchanges.push({ item: current, messages, error: String(e) });
      k(false, String(e));
    });
};
// The chat agent's way: a request with tools, answered with the model's
// message (content and tool calls) as JSON.
globalThis.benchChat = (payloadJson, k) => {
  const { messages, tools } = JSON.parse(payloadJson);
  if (calls >= maxCalls) {
    k(false, "call budget spent");
    return;
  }
  calls++;
  fetch("https://openrouter.ai/api/v1/chat/completions", {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: "Bearer " + key },
    body: JSON.stringify({
      model,
      messages,
      tools,
      temperature: 1.0,
      usage: { include: true },
      ...(reasoning ? { reasoning: { effort: reasoning } } : {}),
    }),
  })
    .then((r) => r.json())
    .then((j) => {
      if (j.error) {
        exchanges.push({ item: current, messages, error: j.error.message });
        k(false, `request failed: ${j.error.message}`);
        return;
      }
      const message = j.choices[0].message;
      const c = (j.usage && j.usage.cost) || 0;
      cost += c;
      exchanges.push({ item: current, messages, reply: message.content || "", tool_calls: message.tool_calls || [], cost: c, usage: j.usage });
      // As it happens, so a check that never returns shows what it was given.
      fs.appendFileSync(out + ".live", JSON.stringify({ item: current, tool_calls: message.tool_calls || [] }) + "\n");
      k(true, JSON.stringify({ content: message.content || "", tool_calls: message.tool_calls || [] }));
    })
    .catch((e) => {
      exchanges.push({ item: current, messages, error: String(e) });
      k(false, String(e));
    });
};
globalThis.benchDone = (json) => {
  const done = JSON.parse(json);
  done.model = model;
  done.intervention = opt("intervention", "none") + (reasoning ? `+reasoning:${reasoning}` : "") + (rounds < 1000 ? `+rounds:${rounds}` : "");
  done.calls = calls;
  done.cost = cost;
  done.exchanges = exchanges;
  fs.writeFileSync(out, JSON.stringify(done, null, 1));
  process.stderr.write(`[bench] done: ${calls} calls, $${cost.toFixed(4)}, -> ${out}\n`);
};
require(path.join(__dirname, "../../_build/default/bench/agentfill/agentfill_bench.bc.js"));
