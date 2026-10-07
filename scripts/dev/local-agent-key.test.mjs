import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import http from "node:http";
import { once } from "node:events";
import { localKeyMiddleware } from "./local-agent-key.mjs";

async function serve(file) {
  const middleware = localKeyMiddleware(file);
  const server = http.createServer((req, res) => middleware(req, res, () => { res.statusCode = 404; res.end(); }));
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  return { server, url: `http://127.0.0.1:${server.address().port}/__hazel_local_key` };
}
const headers = { "X-Hazel-Local-Key": "1", "Content-Type": "application/json" };

test("a key survives server restarts and port changes, is private, and can be forgotten", async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "hazel-key-test-"));
  const file = path.join(dir, "config", "key.json");
  let instance = await serve(file);
  try {
    assert.deepEqual(await (await fetch(instance.url, { headers })).json(), { key: null });
    assert.equal((await fetch(instance.url, { method: "PUT", headers, body: JSON.stringify({ key: " test-only-key " }) })).status, 200);
    assert.equal(fs.statSync(file).mode & 0o777, 0o600);
    assert.equal(fs.statSync(path.dirname(file)).mode & 0o777, 0o700);
    instance.server.close();
    await once(instance.server, "close");
    instance = await serve(file);
    const response = await fetch(instance.url, { headers });
    assert.equal(response.headers.get("cache-control"), "no-store");
    assert.deepEqual(await response.json(), { key: "test-only-key" });
    assert.equal((await fetch(instance.url, { method: "DELETE", headers })).status, 200);
    assert.equal(fs.existsSync(file), false);
    assert.deepEqual(await (await fetch(instance.url, { headers })).json(), { key: null });
  } finally { instance.server.close(); fs.rmSync(dir, { recursive: true, force: true }); }
});

test("rejects foreign origins, rebinding hosts, missing headers and invalid writes", async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "hazel-key-test-"));
  const { server, url } = await serve(path.join(dir, "key.json"));
  try {
    for (const badHeaders of [ {}, { ...headers, Origin: "https://example.com" },
      { ...headers, Origin: "http://localhost:9999" },
      { ...headers, "Sec-Fetch-Site": "cross-site" } ]) {
      assert.equal((await fetch(url, { headers: badHeaders })).status, 403, JSON.stringify(badHeaders));
    }
    // fetch normalizes Host, so use an actual HTTP request for rebinding.
    const reboundStatus = await new Promise((resolve, reject) => {
      http.get(url, { headers: { ...headers, Host: "attacker.example" } }, res => {
        res.resume(); resolve(res.statusCode);
      }).on("error", reject);
    });
    assert.equal(reboundStatus, 403);
    assert.equal((await fetch(url, { method: "OPTIONS", headers })).status, 405);
    assert.equal((await fetch(url, { method: "PUT", headers, body: "invalid" })).status, 400);
    assert.equal((await fetch(url, { method: "PUT", headers, body: '{"key":" "}' })).status, 400);
    assert.equal((await fetch(url, { method: "PUT", headers, body: "x".repeat(5000) })).status, 413);
    assert.deepEqual(await (await fetch(url, { headers })).json(), { key: null });
  } finally { server.close(); fs.rmSync(dir, { recursive: true, force: true }); }
});
