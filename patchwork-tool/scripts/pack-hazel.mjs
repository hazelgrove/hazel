// Ship Hazel inside the Patchwork module: copy Hazel's built web assets
// into dist/hazel/, add the collab boot bundle, and load it from index.html.
//
//   HAZEL_WWW   Hazel's built www dir (default: ../_build/default/src/web/www;
//               build it with `make release` for a small hazel.js)
import { build } from "esbuild";
import { execFileSync } from "node:child_process";
import { cpSync, existsSync, mkdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(here, "../..");
const www = resolve(process.env.HAZEL_WWW ?? join(repoRoot, "_build/default/src/web/www"));
const out = resolve(here, "../dist/hazel");

if (!existsSync(join(www, "hazel.js"))) {
  console.error(`No Hazel build at ${www}. Run \`make release\` (or \`make dev\`) at the repo root first.`);
  process.exit(1);
}

rmSync(out, { recursive: true, force: true });
mkdirSync(out, { recursive: true });
for (const f of ["index.html", "hazel.js", "worker.js", "bundled.js", "style.css", "style", "img", "fonts"]) {
  const src = join(www, f);
  if (existsSync(src)) cpSync(src, join(out, f), { recursive: true, dereference: true });
}
// dune's outputs are read-only; the copies needn't be
execFileSync("chmod", ["-R", "u+w", out]);

await build({
  entryPoints: [join(repoRoot, "collab/src/boot.ts")],
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  minify: true,
  outfile: join(out, "collab.js"),
  nodePaths: [join(repoRoot, "collab/node_modules")],
  logLevel: "warning",
});

// collab.js waits for hazel.js to register its host, and vice versa, so
// load order doesn't matter
const indexPath = join(out, "index.html");
let html = readFileSync(indexPath, "utf8");
if (!html.includes("collab.js")) {
  html = html.replace(
    '<script type="module" src="bundled.js"></script>',
    '<script type="module" src="bundled.js"></script>\n  <script type="module" src="collab.js"></script>',
  );
  writeFileSync(indexPath, html);
}

const mb = (p) => (statSync(p).size / 1e6).toFixed(1) + " MB";
console.log(`packed Hazel from ${www}: hazel.js ${mb(join(out, "hazel.js"))}, collab.js ${mb(join(out, "collab.js"))}`);
