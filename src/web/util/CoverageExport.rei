/* One .coverage file per JS context in a coverage build (see CoverageHook):
   the main thread, the live worker, and each worker WorkerClient restarted.
   Feed the downloaded folder to `make coverage-web-report`. */

/* Saves the files as browser downloads; None outside a coverage build. */
let download: option(unit => unit);

/* Defines window.hazelCoverage in a coverage build: download() as above, and
   files() returning the files as [{name, contents}] for a test harness. A
   no-op in ordinary builds. */
let install: unit => unit;
