# Coverage from browser sessions

Unit-test coverage (`make coverage`) can't see what a person actually exercises
in the editor: views, update cycles, the worker round trip. This workflow
builds Hazel with bisect_ppx instrumentation, lets you use it in a browser, and
turns the session into the same kind of report.

## Usage

### 1. Build and serve an instrumented Hazel

```sh
make coverage-web     # if fmt reformats a file, this fails once; run it again
make serve            # http://localhost:8000
```

It's a dev build with bisect_ppx instrumentation. It looks like ordinary Hazel,
except for an extra download button at the bottom of the sidebar's icon rail.
`make dev` (or any build without `make coverage-web`) puts back an ordinary
build.

### 2. Use Hazel

Do whatever you want covered. Reload the page first if you want the session to
start from zero: the counters cover everything since the page loaded.

### 3. Download the coverage

Click the download button on the sidebar rail, or run
`hazelCoverage.download()` in the browser console. Chrome saves
`hazel-<timestamp>-main.coverage` plus one `hazel-<timestamp>-worker<n>.coverage`
per evaluation worker. It may ask once whether the page can download multiple
files.

### 4. Put the files in a folder of their own

The report reads every `.coverage` file under the folder you give it, so
don't point it at `~/Downloads` itself.

```sh
mkdir -p ~/hazel-coverage/session1
mv ~/Downloads/hazel-*.coverage ~/hazel-coverage/session1/
```

If you downloaded more than once during one page load, keep only the newest
set: each download already includes everything before it.

### 5. Generate and open the report

From the repo root, on the same checkout the build was made from (the files
record source positions):

```sh
make coverage-web-report COVERAGE_DIR=~/hazel-coverage/session1
open _coverage-web/index.html
```

The command prints an overall `Coverage: hit/total (percent)` and writes an
HTML report to `_coverage-web/`. The index lists every file with its percentage.
Clicking a file shows its source, with lines that ran in green and lines that
never ran in red.

### Reading the numbers

- The overall percentage is out of everything instrumented in the bundle,
  including modes and features the session never opened. The per-file numbers
  for the area you were testing are the useful part.
- To combine sessions, point `COVERAGE_DIR` at a parent folder
  (`~/hazel-coverage`): the report sums every file beneath it.

### Scripted sessions

For a scripted session (Playwright, Claude in Chrome), `hazelCoverage.files()`
returns the same files as `[{name, contents}]` without downloading anything.

## How it works

- Every library under `src/` already declares `(instrumentation (backend
  bisect_ppx))`, so `--instrument-with bisect_ppx` instruments the web bundle
  too.
- Under js_of_ocaml, bisect writes nothing at exit. The page pulls the
  counters out itself through `CoverageHook` (`src/web/util/`).
  `HAZEL_WEB_COVERAGE=1` selects the variant that calls `Bisect.Runtime`. Every
  other build gets one that does nothing, so ordinary and release bundles never
  link bisect.
- The main thread and the evaluation worker each have their own counters. The
  worker posts its snapshot to the main thread after every result
  (`ServerMessage.Coverage`). `WorkerClient` keeps the latest snapshot of the
  live worker and the last one from each worker it restarted after a timeout.

## Caveats

- **No reset button.** Reloading is the only way to zero the counters: bisect's
  own `reset_counters` misses the last point of every file. Duplicate downloads
  leave percentages unchanged but double the visit counts.
- **Page load counts.** Startup code shows up as covered in every session.
- **A timed-out evaluation is lost.** When an evaluation times out, the worker
  is terminated before it can report. Everything up to its last completed
  result is kept.
- **Instrumented builds are slower** and the bundles are much larger. Very slow
  evaluations may reach the evaluation timeout sooner than in a normal build.
- **Absent is not 0%.** As with test coverage, a module that is never linked
  appears in neither the numerator nor the denominator.
