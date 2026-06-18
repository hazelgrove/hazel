import { defineConfig, Plugin } from "vite";
import { viteStaticCopy } from "vite-plugin-static-copy";
import fs from "fs";
import path from "path";

function watchExternalPlugin(filePath: string): Plugin {
  return {
    name: "watch-external",
    configureServer(server) {
      const absPath = path.resolve(filePath);
      server.watcher.add(absPath);

      fs.watchFile(absPath, () => {
        const module = server.moduleGraph.getModuleById(absPath);
        if (module) server.moduleGraph.invalidateModule(module);
        server.ws.send({ type: "full-reload", path: "*" });
      });
    },
  };
}

export default defineConfig({
  root: "src/web/www",
  // Cross-origin isolation so test input generation's Z3 WASM (a pthreads
  // build) can use SharedArrayBuffer. `credentialless` keeps cross-origin
  // resources (e.g. Google Fonts) loading without requiring CORP headers.
  server: {
    port: 8000,
    host: true,
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "credentialless",
    },
  },
  preview: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "credentialless",
    },
  },
  plugins: [
    viteStaticCopy({
      targets: [
        { src: "../../../_build/default/src/web/www/worker.js", dest: "" },
        {
          src: "../../../_build/default/src/web/www/bundled.js",
          dest: "",
        },
        { src: "../../../_build/default/src/web/www/hazel.js", dest: "" },
        // Z3 WebAssembly assets for test input generation (copied into the
        // build dir by src/web/www/dune). z3-solver's emscripten loader
        // fetches these relative to the page, so serve them from the www root.
        { src: "../../../_build/default/src/web/www/z3-built.wasm", dest: "" },
        { src: "../../../_build/default/src/web/www/z3-built.js", dest: "" },
      ],
    }),
    watchExternalPlugin("./_build/default/src/web/www/worker.js"),
    watchExternalPlugin("./_build/default/src/web/www/bundled.js"),
    watchExternalPlugin("./_build/default/src/web/www/hazel.js"),
  ],
});
