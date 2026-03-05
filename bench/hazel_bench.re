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

/* Precompute all derived data for a given zipper. */
type fixture = {
  z: Zipper.t,
  segment: Segment.t,
  syntax: CachedSyntax.t,
  term: Exp.t,
  info_map: Statics.Map.t,
};

let make_fixture = (z: Zipper.t): fixture => {
  let segment = Zipper.unselect_and_zip(z);
  let syntax = CachedSyntax.init(z);
  let make_term_result = MakeTerm.go(segment);
  let term = make_term_result.term;
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  {z, segment, syntax, term, info_map};
};

/* --- Benchmark definitions --- */

let tests_for_size = (label: string, program: string): list(Bench.Test.t) => {
  /* Parse once upfront (not benchmarked) */
  let z = parse_to_zipper(program);
  let fix = make_fixture(z);

  [
    Bench.Test.create(~name=label ++ "/MakeTerm.go", () =>
      ignore(MakeTerm.go(fix.segment))
    ),
    Bench.Test.create(~name=label ++ "/Measured.of_segment", () =>
      ignore(
        Measured.of_segment(fix.segment, Id.Map.empty, Id.Map.empty),
      )
    ),
    Bench.Test.create(~name=label ++ "/CachedSyntax.init", () =>
      ignore(CachedSyntax.init(fix.z))
    ),
    Bench.Test.create(~name=label ++ "/Statics.mk", () =>
      ignore(
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          fix.term,
        ),
      )
    ),
    Bench.Test.create(~name=label ++ "/Elaborator.elaborate", () =>
      ignore(Elaborator.elaborate(fix.info_map, fix.term))
    ),
    Bench.Test.create(~name=label ++ "/Zipper.move(Left)", () =>
      ignore(Zipper.move(Left, fix.z))
    ),
    Bench.Test.create(~name=label ++ "/Move.go(Left,ByChar)", () =>
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
    Bench.Test.create(~name=label ++ "/CachedSyntax.rebuild", () => {
      let old = CachedSyntax.mark_old(fix.syntax);
      ignore(
        CachedSyntax.calculate(fix.z, Id.Map.empty, Id.Map.empty, old),
      );
    }),
  ];
};

/* --- JSON output --- */

let output_json = (results: list(Bench.Analysis_result.t)): unit => {
  let entries =
    List.map(results, ~f=r => {
      let time_per_run =
        switch (
          Array.find(Bench.Analysis_result.regressions(r), ~f=reg =>
            Poly.(==)(Bench.Analysis_result.Regression.responder(reg), `Nanos)
          )
        ) {
        | Some(reg) =>
          switch (
            Array.find(Bench.Analysis_result.Regression.coefficients(reg), ~f=c =>
              Poly.(==)(Bench.Analysis_result.Coefficient.predictor(c), `Runs)
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
    });
  print_endline("[");
  print_endline(String.concat(entries, ~sep=",\n"));
  print_endline("]");
};

/* --- Main --- */

let () = {
  let json_mode = Array.exists(Sys.get_argv(), ~f=s => String.equal(s, "--json"));

  /* Define all benchmarks */
  let tests =
    tests_for_size("let100", gen_let_chain(100))
    @ tests_for_size("let500", gen_let_chain(500))
    @ tests_for_size("case100", gen_case_chain(100));

  if (json_mode) {
    let run_config =
      Bench.Run_config.create(~quota=Bench.Quota.Num_calls(20), ());
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
    /* Use core_bench's built-in command-line interface for interactive use.
     * This respects -quota, -ci-absolute, etc. passed via argv.
     * Since we can't use Command.run in JS, call bench directly. */
    let run_config =
      Bench.Run_config.create(~quota=Bench.Quota.Num_calls(30), ());
    Bench.bench(~run_config, tests);
  };
};
