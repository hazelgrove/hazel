open Alcotest;
open Language;

/* Tests for Haz3lcore.Reach: path-condition extraction + end-to-end solve.
 * We mark a reach point by a unique integer literal in the program, find its
 * node id, run Reach.analyze, and solve the resulting script with the native
 * z3 backend. Native-only (testgenZ3); solve cases guard on z3 availability. */

module TG = Haz3lcore.TestGen;
module Reach = Haz3lcore.Reach;

/* Find the node id of the (assumed unique) integer literal `n`. */
let find_int_lit = (n: int, map: Statics.Map.t): option(Id.t) =>
  Id.Map.fold(
    (id, info, acc) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        switch (info) {
        | Statics.Info.InfoExp({user_term, _}) =>
          switch (user_term.term) {
          | Atom(Int(v)) when Bigint.to_int(v) == Some(n) => Some(id)
          | _ => None
          }
        | _ => None
        }
      },
    map,
    None,
  );

let analyze_lit = (src: string, n: int): option(Reach.t) =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, src)) {
  | None => None
  | Some(zipper) =>
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    let (map, _) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    switch (find_int_lit(n, map)) {
    | Some(id) => Reach.analyze(id, map)
    | None => None
    };
  };

let outcome_of = (r: Reach.t): TG.outcome => {
  let (script, _complete) = Reach.smtlib2(r);
  TestgenZ3.Z3Native.solve(script);
};

/* Two reach points from the same parse (so variable names line up). */
let analyze_two =
    (src: string, n1: int, n2: int): option((Reach.t, Reach.t)) =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, src)) {
  | None => None
  | Some(zipper) =>
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    let (map, _) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    switch (find_int_lit(n1, map), find_int_lit(n2, map)) {
    | (Some(i1), Some(i2)) =>
      switch (Reach.analyze(i1, map), Reach.analyze(i2, map)) {
      | (Some(r1), Some(r2)) => Some((r1, r2))
      | _ => None
      }
    | _ => None
    };
  };

/* === pure: path-condition shape (no solver) === */

let pure_tests = [
  test_case("guard count in nested if", `Quick, () =>
    switch (analyze_lit("if x > 0 then if x < 5 then 1 else 2 else 3", 1)) {
    | Some(r) =>
      check(int, "two guards to innermost then", 2, List.length(r.guards))
    | None => fail("analyze returned None")
    }
  ),
  test_case("else-branch reach point still has a guard", `Quick, () =>
    switch (analyze_lit("if x > 0 then 1 else 2", 2)) {
    | Some(r) => check(int, "one (negated) guard", 1, List.length(r.guards))
    | None => fail("analyze returned None")
    }
  ),
  test_case("let binding captured in scope", `Quick, () =>
    switch (analyze_lit("let t = x + x in if t > 10 then 5 else 0", 5)) {
    | Some(r) =>
      check(bool, "t recorded in scope", true, List.mem_assoc("t", r.lets))
    | None => fail("analyze returned None")
    }
  ),
];

/* === end-to-end solve (needs z3) === */

let solve_tests =
  if (!TestgenZ3.Z3Native.is_available()) {
    [
      test_case("z3 unavailable", `Quick, () =>
        check(bool, "skipped", true, true)
      ),
    ];
  } else {
    [
      test_case("then-branch is reachable", `Quick, () =>
        switch (analyze_lit("if x > 0 then 1 else 2", 1)) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("analyze returned None")
        }
      ),
      test_case("contradictory nested branch is dead code", `Quick, () =>
        switch (analyze_lit("if x > 0 then if x < 0 then 7 else 1 else 2", 7)) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Unsat => check(bool, "unreachable", true, true)
          | other => fail("expected Unsat, got " ++ TG.show_outcome(other))
          }
        | None => fail("analyze returned None")
        }
      ),
      test_case("reach through a let binding", `Quick, () =>
        switch (analyze_lit("let t = x + x in if t > 10 then 5 else 0", 5)) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("analyze returned None")
        }
      ),
      test_case(
        "merge of mutually-exclusive branches is incompatible", `Quick, () =>
        switch (analyze_two("if x > 0 then 1 else 2", 1, 2)) {
        | Some((r1, r2)) =>
          switch (outcome_of(Reach.merge([r1, r2]))) {
          | Unsat => check(bool, "incompatible", true, true)
          | other => fail("expected Unsat, got " ++ TG.show_outcome(other))
          }
        | None => fail("analyze_two returned None")
        }
      ),
      test_case("merge of compatible branches is satisfiable", `Quick, () =>
        switch (
          analyze_two(
            "(if x > 0 then 1 else 9, if x > 5 then 2 else 8)",
            1,
            2,
          )
        ) {
        | Some((r1, r2)) =>
          switch (outcome_of(Reach.merge([r1, r2]))) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("analyze_two returned None")
        }
      ),
    ];
  };

let tests = ("Reach", pure_tests @ solve_tests);
