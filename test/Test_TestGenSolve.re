open Alcotest;
open Language;

/* End-to-end test of the native Z3 backend: parse a Hazel boolean expression,
 * run statics, build SMT-LIB2 (TestGen.build), and solve it by shelling out to
 * the system `z3` binary (TestgenZ3.Z3Native). Lives only in the native test
 * exe (testgenZ3 is native-only). Skipped when no `z3` binary is available. */

module TG = Haz3lcore.TestGen;

/* Statics.Info.exp for the root of a parsed program; mirrors the CLI's flow. */
let root_info = (src: string): option(Statics.Info.exp) =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, src)) {
  | None => None
  | Some(zipper) =>
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    let (map, _) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    let root_id =
      Haz3lcore.Segment.root_id(Haz3lcore.Segment.skel(segment), segment);
    switch (Statics.Map.lookup(root_id, map)) {
    | Some(InfoExp(e)) => Some(e)
    | _ => None
    };
  };

let solve_src = (src: string): TG.outcome =>
  switch (root_info(src)) {
  | None => TG.Error("parse/statics failure for: " ++ src)
  | Some(e) => TestgenZ3.Z3Native.solve_info(e)
  };

let lookup =
    (assignments: list(TG.assignment), name: string): option(string) =>
  List.find_map(
    (a: TG.assignment) => a.name == name ? Some(a.value) : None,
    assignments,
  );

let tests =
  if (!TestgenZ3.Z3Native.is_available()) {
    (
      "TestGenSolve (skipped: no z3 binary)",
      [
        test_case("z3 unavailable", `Quick, () =>
          check(bool, "skipped", true, true)
        ),
      ],
    );
  } else {
    (
      "TestGenSolve",
      [
        test_case("range predicate is satisfiable", `Quick, () =>
          switch (solve_src("x > 5 && x < 10")) {
          | Sat(assignments) =>
            switch (lookup(assignments, "x")) {
            | Some(v) =>
              let n = int_of_string(v);
              check(bool, "5 < x < 10", true, n > 5 && n < 10);
            | None => fail("no assignment for x")
            }
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        ),
        test_case("contradiction is unsatisfiable", `Quick, () =>
          switch (solve_src("x > 10 && x < 5")) {
          | Unsat => check(bool, "unsat", true, true)
          | other => fail("expected Unsat, got " ++ TG.show_outcome(other))
          }
        ),
      ],
    );
  };
