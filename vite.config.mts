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
  server: {
    port: 8000,
    host: true,
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
      ],
    }),
    watchExternalPlugin("./_build/default/src/web/www/worker.js"),
    watchExternalPlugin("./_build/default/src/web/www/bundled.js"),
    watchExternalPlugin("./_build/default/src/web/www/hazel.js"),
  ],
});
