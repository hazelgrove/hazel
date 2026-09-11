import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { resolve } from "path";
import dts from "vite-plugin-dts";

// https://vite.dev/config/
export default defineConfig({
  plugins: [
    react(),
    dts({
      include: ["src/**/*.ts", "src/**/*.tsx"],
      exclude: ["src/main.tsx", "src/App.tsx"],
    }),
  ],
  // if dev, /, otherwise, hazel-embed
  base: process.env.NODE_ENV === "development" ? "/" : "/hazel-embed/",
  server: {
    port: 8081,
    host: "0.0.0.0",
    allowedHosts: true,
  },
  build: {
    lib: {
      entry: resolve(__dirname, "src/index.ts"),
      name: "HazelEmbed",
      fileName: (format) => `hazel-embed.${format === "es" ? "js" : "umd.js"}`,
      formats: ["es", "umd"],
    },
    rollupOptions: {
      external: ["react", "react-dom"],
      output: {
        globals: {
          react: "React",
          "react-dom": "ReactDOM",
        },
      },
    },
    sourcemap: true,
    minify: true,
  },
});
