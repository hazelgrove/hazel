/* Access to this JS context's bisect_ppx counters (the main thread and the
   worker each have their own). Only a `make coverage-web` build has them: the
   rules in src/web/dune pick CoverageHook.enabled.re there and
   CoverageHook.disabled.re otherwise, so ordinary builds never link bisect.

   Some(snapshot) in a coverage build; snapshot() is the counters so far in
   .coverage file format, or None if no instrumented module has registered. */
let snapshot: option(unit => option(string));
