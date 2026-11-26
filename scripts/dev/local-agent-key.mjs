import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { randomUUID } from "node:crypto";

export const keyPath = path.join(os.homedir(), ".config", "hazel", "openrouter-api-key.json");
const loopback = new Set(["127.0.0.1", "::1", "::ffff:127.0.0.1"]);

// Dev-only credential storage shared by local Hazel ports/worktrees. Never
// expose this through the static root, the production bundle, or CORS.
export function localKeyMiddleware(file = keyPath) {
  return (req, res, next) => {
    if (req.url !== "/__hazel_local_key") return next();
    res.setHeader("Cache-Control", "no-store");
    res.setHeader("Content-Type", "application/json");
    res.removeHeader("Access-Control-Allow-Origin");
    const reply = (code, body) => { res.statusCode = code; res.end(JSON.stringify(body)); };
    const host = req.headers.host || "";
    const localHost = /^(localhost|127\.0\.0\.1|\[::1\])(:\d+)?$/.test(host);
    const origin = `${req.socket.encrypted ? "https" : "http"}://${host}`;
    if (!loopback.has(req.socket.remoteAddress) || !localHost ||
        req.headers["x-hazel-local-key"] !== "1" ||
        (req.headers.origin && req.headers.origin !== origin) ||
        (req.headers["sec-fetch-site"] && req.headers["sec-fetch-site"] !== "same-origin")) {
      return reply(403, { error: "Local, same-origin requests only" });
    }
    const failed = () => reply(500, { error: "Could not access local key storage" });
    if (req.method === "GET") {
      try {
        const key = fs.existsSync(file) ? JSON.parse(fs.readFileSync(file, "utf8")).key : null;
        return reply(200, { key: typeof key === "string" ? key : null });
      } catch { return failed(); }
    }
    if (req.method === "DELETE") {
      try { fs.rmSync(file, { force: true }); return reply(200, { key: null }); }
      catch { return failed(); }
    }
    if (req.method !== "PUT") return reply(405, { error: "Method not allowed" });
    if (!req.headers["content-type"]?.startsWith("application/json")) {
      return reply(415, { error: "Expected JSON" });
    }
    let body = "", bytes = 0;
    req.on("data", chunk => {
      bytes += chunk.length;
      if (bytes <= 4096) body += chunk;
    });
    req.on("end", () => {
      if (bytes > 4096) return reply(413, { error: "Key too large" });
      let key;
      try { key = JSON.parse(body).key; } catch { return reply(400, { error: "Invalid JSON" }); }
      if (typeof key !== "string" || !key.trim() || key.length > 2048) {
        return reply(400, { error: "Expected a non-empty key" });
      }
      const temp = `${file}.${randomUUID()}.tmp`;
      try {
        fs.mkdirSync(path.dirname(file), { recursive: true, mode: 0o700 });
        fs.writeFileSync(temp, JSON.stringify({ key: key.trim() }) + "\n", { mode: 0o600, flag: "wx" });
        fs.renameSync(temp, file);
        return reply(200, { saved: true });
      } catch {
        try { fs.rmSync(temp, { force: true }); } catch { /* Best-effort cleanup. */ }
        return failed();
      }
    });
  };
}

export function localAgentKeyPlugin() {
  return {
    name: "hazel-local-agent-key",
    configureServer(server) { server.middlewares.use(localKeyMiddleware()); },
  };
}
