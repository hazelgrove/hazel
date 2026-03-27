import options from "./options.js";
import * as esbuild from "esbuild";
const context = await esbuild.context(options);
await context.watch();
