import { defineConfig } from "vite";
import cssInjectedByJsPlugin from "vite-plugin-css-injected-by-js";
import externals from "@inkandswitch/patchwork-bootloader/externals";

// Patchwork packages, automerge, solid, etc. come from the host's importmap;
// everything else is bundled. CSS is injected from the chunk that imports it
// (relativeCSSInjection), since the entry module runs in a worker with no
// `document`.
export default defineConfig({
  base: "./",
  plugins: [cssInjectedByJsPlugin({ relativeCSSInjection: true })],
  build: {
    sourcemap: true,
    cssCodeSplit: true,
    emptyOutDir: true,
    minify: false,
    rollupOptions: {
      external: externals,
      input: "./src/index.ts",
      output: {
        format: "es",
        entryFileNames: "[name].js",
        // chunks sit next to index.js so the tool can find ./hazel/
        chunkFileNames: "[name]-[hash].js",
        assetFileNames: "assets/[name][extname]",
      },
      preserveEntrySignatures: "strict",
    },
  },
});
