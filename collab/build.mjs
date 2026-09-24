// Bundle the in-page boot script (Automerge + wasm included) into
// dist/collab.js, which the Patchwork tool ships next to Hazel's build.
import { build } from "esbuild";

await build({
  entryPoints: ["src/boot.ts"],
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  outfile: "dist/collab.js",
  sourcemap: true,
  logLevel: "info",
});
