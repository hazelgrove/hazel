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

/* Constellation traces: the canvas keeps recorded agent runs in the
   browser; "keep" POSTs one here and it lands in <root>/trajectories as a
   file every tab can replay (GET /__traces lists them). Dev server only. */
function tracesPlugin(): Plugin {
  return {
    name: "canvas-traces",
    configureServer(server) {
      const dir = path.resolve(server.config.root, "trajectories");
      server.middlewares.use("/__traces", (req, res) => {
        try {
          fs.mkdirSync(dir, { recursive: true });
          if (req.method === "GET") {
            const files = fs
              .readdirSync(dir)
              .filter((f) => f.endsWith(".json"))
              .map((f) => {
                const st = fs.statSync(path.join(dir, f));
                return { name: f, size: st.size, mtime: st.mtimeMs };
              })
              .sort((a, b) => b.mtime - a.mtime);
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify(files));
            return;
          }
          if (req.method === "POST") {
            const name = decodeURIComponent((req.url || "/").slice(1)).replace(
              /[^A-Za-z0-9_.-]/g,
              "_",
            );
            if (!name.endsWith(".json") || name.length < 6) {
              res.statusCode = 400;
              res.end("name must end in .json");
              return;
            }
            let body = "";
            req.on("data", (c) => (body += c));
            req.on("end", () => {
              fs.writeFileSync(path.join(dir, name), body);
              res.end("ok");
            });
            return;
          }
          res.statusCode = 405;
          res.end();
        } catch (e) {
          res.statusCode = 500;
          res.end(String(e));
        }
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
    tracesPlugin(),
  ],
});
