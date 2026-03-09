/* Hazel performance benchmarks using core_bench.
 *
 * Measures key operations at various program sizes.
 * Outputs results to stdout as a table (default) or as JSON (--json flag).
 *
 * Usage:
 *   node bench/hazel_bench.bc.js            # table output
 *   node bench/hazel_bench.bc.js --json     # JSON output for CI comparison
 *   node bench/hazel_bench.bc.js --help     # core_bench options
 */

open Core;
module Bench = Core_bench_js;
open Haz3lcore;
open Language;

/* --- Program generators --- */

/* Generate a let-chain program string with n bindings.
 * Each binding adds ~5-10 AST nodes. */
let gen_let_chain = (n: int): string => {
  let buf = Stdlib.Buffer.create(n * 30);
  for (i in 0 to n - 1) {
    if (i == 0) {
      Stdlib.Buffer.add_string(buf, "let x0 = 0 in\n");
    } else {
      Stdlib.Buffer.add_string(
        buf,
        "let x"
        ++ string_of_int(i)
        ++ " = x"
        ++ string_of_int(i - 1)
        ++ " + 1 in\n",
      );
    };
  };
  Stdlib.Buffer.add_string(buf, "x" ++ string_of_int(n - 1));
  Stdlib.Buffer.contents(buf);
};

/* Generate a program with nested case expressions.
 * Each function adds ~15 AST nodes. */
let gen_case_chain = (n: int): string => {
  let buf = Stdlib.Buffer.create(n * 80);
  for (i in 0 to n - 1) {
    let prev =
      if (i == 0) {
        "x";
      } else {
        "f" ++ string_of_int(i - 1) ++ "(x)";
      };
    Stdlib.Buffer.add_string(
      buf,
      "let f"
      ++ string_of_int(i)
      ++ " = fun x -> case x | 0 => 0 | _ => "
      ++ prev
      ++ " end in\n",
    );
  };
  Stdlib.Buffer.add_string(buf, "f" ++ string_of_int(n - 1) ++ "(0)");
  Stdlib.Buffer.contents(buf);
};

/* --- Precomputed fixtures --- */

/* Parse a program string into a zipper (expensive, done once per size). */
let parse_to_zipper = (program: string): Zipper.t =>
  switch (Parser.to_zipper(program)) {
  | Some(z) => z
  | None => failwith("Failed to parse: " ++ String.prefix(program, 40))
  };

/* Precompute all derived data for a given zipper, including a
 * post-edit snapshot so downstream phases can be benchmarked
 * individually with realistic (post-edit) inputs. */
type fixture = {
  z: Zipper.t,
  segment: Segment.t,
  syntax: CachedSyntax.t,
  term: Exp.t,
  info_map: Statics.Map.t,
  /* Post-edit state (Insert "x" applied once) */
  edited_segment: Segment.t,
  edited_term: Exp.t,
  edited_info_map: Statics.Map.t,
  edited_dhexp: DHExp.t,
};

let make_fixture = (z: Zipper.t): fixture => {
  let segment = Zipper.unselect_and_zip(z);
  let syntax = CachedSyntax.init(z);
  let make_term_result = MakeTerm.go(segment);
  let term = make_term_result.term;
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  /* Compute post-edit state for individual phase benchmarks */
  let edited_z =
    switch (
      Perform.go(
        ~statics=CachedStatics.empty,
        ~syntax,
        Insert("x"),
        {zipper: z, col_target: None},
      )
    ) {
    | Ok(z) => z
    | Error(_) => z
    };
  let edited_segment = Zipper.unselect_and_zip(edited_z);
  let edited_make_term = MakeTerm.go(edited_segment);
  let edited_term = edited_make_term.term;
  let edited_info_map =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      edited_term,
    );
  let (edited_dhexp, _) =
    Elaborator.elaborate(edited_info_map, edited_term);
  {
    z,
    segment,
    syntax,
    term,
    info_map,
    edited_segment,
    edited_term,
    edited_info_map,
    edited_dhexp,
  };
};

/* --- Benchmark definitions --- */

/* Repeated-call benchmarks: measure memo-hit overhead.
 * These call functions with the same inputs on every iteration,
 * so Core.Memo.general will cache the result after the first call.
 * Useful for understanding the cost of memo lookup itself. */
let memo_tests_for_size =
    (label: string, program: string): list(Bench.Test.t) => {
  let z = parse_to_zipper(program);
  let fix = make_fixture(z);

  [
    Bench.Test.create(~name=label ++ "/memo/MakeTerm.go", () =>
      ignore(MakeTerm.go(fix.segment))
    ),
    Bench.Test.create(~name=label ++ "/memo/Measured.of_segment", () =>
      ignore(Measured.of_segment(fix.segment, Id.Map.empty, Id.Map.empty))
    ),
    Bench.Test.create(~name=label ++ "/memo/Statics.mk", () =>
      ignore(
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), fix.term),
      )
    ),
    Bench.Test.create(~name=label ++ "/memo/Elaborator.elaborate", () =>
      ignore(Elaborator.elaborate(fix.info_map, fix.term))
    ),
  ];
};

/* Edit-cycle benchmarks: measure individual pipeline phases.
 *
 * Each phase is benchmarked in isolation using pre-computed inputs
 * from the previous phase (computed once in make_fixture). This means
 * iteration 1 is a cold call but iterations 2+ may hit memo caches
 * since the inputs don't change between iterations.
 *
 * Pipeline: Perform -> MakeTerm -> Measured -> Statics -> Elaborate -> Evaluate
 *
 * The compare.js script computes a "Total" row by summing all phases. */
let edit_cycle_tests_for_size =
    (label: string, program: string): list(Bench.Test.t) => {
  let z = parse_to_zipper(program);
  let fix = make_fixture(z);
  let statics = CachedStatics.empty;

  [
    Bench.Test.create(~name=label ++ "/edit/Perform", () =>
      ignore(
        Perform.go(
          ~statics,
          ~syntax=fix.syntax,
          Insert("x"),
          {zipper: fix.z, col_target: None},
        ),
      )
    ),
    Bench.Test.create(~name=label ++ "/edit/MakeTerm", () =>
      ignore(MakeTerm.go(fix.edited_segment))
    ),
    Bench.Test.create(~name=label ++ "/edit/Measured", () =>
      ignore(
        Measured.of_segment(
          fix.edited_segment,
          Id.Map.empty,
          Id.Map.empty,
        ),
      )
    ),
    Bench.Test.create(~name=label ++ "/edit/Statics", () =>
      ignore(
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          fix.edited_term,
        ),
      )
    ),
    Bench.Test.create(~name=label ++ "/edit/Elaborate", () =>
      ignore(Elaborator.elaborate(fix.edited_info_map, fix.edited_term))
    ),
    Bench.Test.create(~name=label ++ "/edit/Evaluate", () =>
      ignore(
        Evaluator.evaluate(~env=Builtins.env_init, fix.edited_dhexp),
      )
    ),
    Bench.Test.create(~name=label ++ "/edit/Move(Left)", () =>
      ignore(
        Move.go(
          ~statics=fix.info_map,
          ~col_target=0,
          ~measured=fix.syntax.measured,
          Local(Left, ByChar),
          fix.z,
        ),
      )
    ),
  ];
};

/* --- JSON output --- */

let output_json = (results: list(Bench.Analysis_result.t)): unit => {
  let entries =
    List.map(
      results,
      ~f=r => {
        let time_per_run =
          switch (
            Array.find(Bench.Analysis_result.regressions(r), ~f=reg =>
              Poly.(==)(
                Bench.Analysis_result.Regression.responder(reg),
                `Nanos,
              )
            )
          ) {
          | Some(reg) =>
            switch (
              Array.find(
                Bench.Analysis_result.Regression.coefficients(reg), ~f=c =>
                Poly.(==)(
                  Bench.Analysis_result.Coefficient.predictor(c),
                  `Runs,
                )
              )
            ) {
            | Some(c) => Bench.Analysis_result.Coefficient.estimate(c)
            | None => Float.nan
            }
          | None => Float.nan
          };
        Printf.sprintf(
          {|  {"name": "%s", "time_ns": %.2f, "samples": %d}|},
          Bench.Analysis_result.name(r),
          time_per_run,
          Bench.Analysis_result.sample_count(r),
        );
      },
    );
  print_endline("[");
  print_endline(String.concat(entries, ~sep=",\n"));
  print_endline("]");
};

/* --- Main --- */

/* Parse --filter <pattern> from argv. Matches benchmark names containing
 * the pattern as a substring (case-sensitive). Multiple --filter flags
 * are OR'd together. Examples:
 *   --filter let500           # all let500 benchmarks
 *   --filter Insert+Full      # just the full edit cycle
 *   --filter memo             # all memo-hit benchmarks
 *   --filter let500/edit      # let500 edit cycle benchmarks */
let parse_filters = (argv: array(string)): list(string) => {
  let filters = ref([]);
  let arr = Array.to_list(argv);
  let rec go =
    fun
    | [] => ()
    | ["--filter", pattern, ...rest] => {
        filters := [pattern, ...filters^];
        go(rest);
      }
    | [_, ...rest] => go(rest);
  go(arr);
  filters^;
};

let () = {
  let argv = Sys.get_argv();
  let json_mode = Array.exists(argv, ~f=s => String.equal(s, "--json"));
  let filters = parse_filters(argv);

  /* Define all benchmarks */
  let tests =
    edit_cycle_tests_for_size("let100", gen_let_chain(100))
    @ edit_cycle_tests_for_size("let500", gen_let_chain(500))
    @ edit_cycle_tests_for_size("case100", gen_case_chain(100))
    @ memo_tests_for_size("let100", gen_let_chain(100))
    @ memo_tests_for_size("let500", gen_let_chain(500));

  /* Apply filters if any were specified */
  let tests =
    switch (filters) {
    | [] => tests
    | _ =>
      List.filter(tests, ~f=t =>
        List.exists(filters, ~f=pattern =>
          String.is_substring(Bench.Test.name(t), ~substring=pattern)
        )
      )
    };

  if (List.is_empty(tests)) {
    Printf.eprintf(
      "No benchmarks matched filter(s): %s\n",
      String.concat(filters, ~sep=", "),
    );
  } else if (json_mode) {
    let run_config =
      Bench.Run_config.create(
        ~quota=Bench.Quota.Num_calls(20),
        ~stabilize_gc_between_runs=true,
        (),
      );
    let measurements = Bench.measure(~run_config, tests);
    let results =
      List.filter_map(measurements, ~f=m =>
        switch (Bench.analyze(m)) {
        | Ok(r) => Some(r)
        | Error(_) => None
        }
      );
    output_json(results);
  } else {
    let run_config =
      Bench.Run_config.create(
        ~quota=Bench.Quota.Num_calls(30),
        ~stabilize_gc_between_runs=true,
        (),
      );
    Bench.bench(~run_config, tests);
  };
};
