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
});

let current = null;
let calls = 0;
const exchanges = [];
let cost = 0;
globalThis.benchProgress = (name) => {
  current = name;
  process.stderr.write(`[bench] ${name}\n`);
};
globalThis.benchAsk = (messagesJson, k) => {
  const messages = JSON.parse(messagesJson);
  if (calls >= maxCalls) {
    k(false, "call budget spent");
    return;
  }
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
    body: JSON.stringify({ model, messages, temperature: 1.0, usage: { include: true } }),
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
      exchanges.push({ item: current, messages, reply, cost: c });
      k(true, reply);
    })
    .catch((e) => {
      exchanges.push({ item: current, messages, error: String(e) });
      k(false, String(e));
    });
};
globalThis.benchDone = (json) => {
  const done = JSON.parse(json);
  done.model = model;
  done.intervention = opt("intervention", "none");
  done.calls = calls;
  done.cost = cost;
  done.exchanges = exchanges;
  fs.writeFileSync(out, JSON.stringify(done, null, 1));
  process.stderr.write(`[bench] done: ${calls} calls, $${cost.toFixed(4)}, -> ${out}\n`);
};
require(path.join(__dirname, "../../_build/default/bench/agentfill/agentfill_bench.bc.js"));
