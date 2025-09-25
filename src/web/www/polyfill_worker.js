const Worker = require("web-worker");
const path = require("path");
global.Worker = Worker;

const workerPath = path.join(process.cwd(), "_build/default/src/web/worker.bc.js");

const worker = new Worker(workerPath, { type: "module" });
