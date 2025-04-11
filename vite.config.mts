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
  root: "src/haz3lweb/www",
  server: {
    port: 8000,
    host: true,
  },
  plugins: [
    viteStaticCopy({
      targets: [
        { src: "../../../_build/default/src/haz3lweb/www/worker.js", dest: "" },
        {
          src: "../../../_build/default/src/haz3lweb/www/bundled.js",
          dest: "",
        },
        { src: "../../../_build/default/src/haz3lweb/www/hazel.js", dest: "" },
      ],
    }),
    watchExternalPlugin("./_build/default/src/haz3lweb/www/worker.js"),
    watchExternalPlugin("./_build/default/src/haz3lweb/www/bundled.js"),
    watchExternalPlugin("./_build/default/src/haz3lweb/www/hazel.js"),
  ],
});
