// A trial, not for merging (trial/2606-timing): what it costs to copy a message of each size across postMessage to a
// worker thread and back to a reply, in Node's V8 (Chrome's engine), for the sizes test/Test_HeldCacheTiming.re measured.
// A Marshal-encoded message is a one-byte string; the time is the round trip less that of a 1-byte message, the median
// of 51 trips.  node trial/postmessage-copy.mjs heldtiming.out
import { Worker } from "node:worker_threads";
import { readFileSync } from "node:fs";
const sizes = new Map();
for (const line of readFileSync(process.argv[2], "utf8").split("\n")) {
  const m = line.match(/^HELDTIMING (\S+) (request|response) encode (dev|2606) bytes=(\d+)/);
  if (m) sizes.set(`${m[1]} ${m[2]} ${m[3]}`, Number(m[4]));
}
const w = new Worker("const { parentPort } = require('node:worker_threads'); parentPort.on('message', s => parentPort.postMessage(s.length));", { eval: true });
const trip = s => new Promise(ok => { const t0 = performance.now(); w.once("message", () => ok(performance.now() - t0)); w.postMessage(s); });
const median = xs => xs.slice().sort((a, b) => a - b)[xs.length >> 1];
const time = async s => { for (let i = 0; i < 5; i++) await trip(s); const xs = []; for (let i = 0; i < 51; i++) xs.push(await trip(s)); return median(xs); };
const base = await time("x");
for (const [k, n] of sizes) {
  const s = "ÿ".repeat(1) + String.fromCharCode(...Array.from({ length: 256 }, (_, i) => i)).repeat(Math.ceil(n / 256)).slice(0, n - 1);
  const t = await time(s);
  console.log(`COPY ${k} bytes=${n} ms=${Math.max(0, t - base).toFixed(3)}`);
}
console.log(`COPY baseline 1-byte round trip ms=${base.toFixed(3)}`);
await w.terminate();
