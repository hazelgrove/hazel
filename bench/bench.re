/* Performance benchmarks for Hazel core pipeline.
   Run: dune build bench && node _build/default/bench/bench.bc.js

   TODO: stubbed — see merge brief.
   Original bench.re (preserved as .merge-stash/bench.re.preserved at the
   repo root) used the old Elaborator module + old Statics.mk signature,
   both of which were refactored on dev (Elaborator.re deleted; Statics.mk
   now returns (info_map, elaborated)). The original file is ~860 lines of
   profiling glue; rewriting it for the new API is left to the user. */

let () = {
  Printf.printf("[BENCH] Hazel benchmark suite is currently stubbed.\n%!");
  Printf.printf(
    "[BENCH] See .merge-stash/bench.re.preserved for the original (broken).\n%!",
  );
};
