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
  test_case("irrelevant let is pruned (stays complete)", `Quick, () =>
    switch (analyze_lit("let z = 100 in if x > 5 then 1 else 0", 1)) {
    | Some(r) =>
      check(bool, "z pruned", false, List.mem_assoc("z", r.lets));
      let (_, complete) = Reach.smtlib2(r);
      check(bool, "complete", true, complete);
    | None => fail("analyze returned None")
    }
  ),
  test_case("function params become inputs (call site ignored)", `Quick, () =>
    switch (
      analyze_lit(
        "let f = fun (a, b) -> if a > b then 1 else 2 in f(3, 4)",
        1,
      )
    ) {
    | Some(r) => check(bool, "a is an input", true, List.mem("a", r.inputs))
    | None => fail("analyze returned None")
    }
  ),
];

/* A larger program used both as a demo and to exercise nested if/let, a
   tuple-pattern function parameter, and a genuinely dead branch. */
let demo_program = {|
let price = fun (qty, vip) ->
  let base = qty * 10 in
  let discount =
    if vip then
      if qty > 100 then
        if qty < 50 then 99 else 30
      else 10
    else
      if qty > 50 then 5 else 0
  in
  let shipping = if base > 500 then 0 else 25 in
  base - discount + shipping
in
price(7, true)
|};

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
      test_case("demo program: contradictory branch is dead", `Quick, () =>
        switch (analyze_lit(demo_program, 99)) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Unsat => check(bool, "dead", true, true)
          | other => fail("expected Unsat, got " ++ TG.show_outcome(other))
          }
        | None => fail("demo analyze returned None (parse?)")
        }
      ),
      test_case("demo program: deep branch is reachable", `Quick, () =>
        switch (analyze_lit(demo_program, 30)) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "reachable", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("demo analyze returned None (parse?)")
        }
      ),
      test_case("match: a literal arm is reachable", `Quick, () =>
        switch (analyze_lit("case n | 0 => 1 | 1 => 2 | _ => 3 end", 2)) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("match analyze returned None (parse?)")
        }
      ),
      test_case("match arm under a contradictory guard is dead", `Quick, () =>
        switch (
          analyze_lit("if n > 10 then case n | 0 => 1 | _ => 2 end else 3", 1)
        ) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Unsat => check(bool, "dead", true, true)
          | other => fail("expected Unsat, got " ++ TG.show_outcome(other))
          }
        | None => fail("match analyze returned None (parse?)")
        }
      ),
      test_case("application of a let-bound function is inlined", `Quick, () =>
        switch (
          analyze_lit(
            "let double = fun x -> x * 2 in if double(a) > 10 then 1 else 2",
            1,
          )
        ) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("ap analyze returned None (parse?)")
        }
      ),
      test_case("inlined function yields a dead branch", `Quick, () =>
        switch (
          analyze_lit(
            "let zero = fun x -> x * 0 in if zero(a) > 5 then 1 else 2",
            1,
          )
        ) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Unsat => check(bool, "dead", true, true)
          | other => fail("expected Unsat, got " ++ TG.show_outcome(other))
          }
        | None => fail("ap analyze returned None (parse?)")
        }
      ),
      test_case("inlining keeps a captured value binding", `Quick, () =>
        switch (
          analyze_lit(
            "let c = 100 in let f = fun x -> x + c in if f(a) > 150 then 1 else 2",
            1,
          )
        ) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("ap analyze returned None (parse?)")
        }
      ),
      test_case("tuple let-binding is decomposed", `Quick, () =>
        switch (
          analyze_lit("let (a, b) = (x, 5) in if a > b then 1 else 2", 1)
        ) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("tuple-let analyze returned None (parse?)")
        }
      ),
      test_case("tuple match pattern matches component-wise", `Quick, () =>
        switch (analyze_lit("case (n, m) | (0, 5) => 1 | _ => 2 end", 1)) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("tuple-match analyze returned None (parse?)")
        }
      ),
      test_case("tuple-parameter function is inlined", `Quick, () =>
        switch (
          analyze_lit(
            "let f = fun (a, b) -> a > b in if f(x, 5) then 1 else 2",
            1,
          )
        ) {
        | Some(r) =>
          switch (outcome_of(r)) {
          | Sat(_) => check(bool, "sat", true, true)
          | other => fail("expected Sat, got " ++ TG.show_outcome(other))
          }
        | None => fail("tuple-param analyze returned None (parse?)")
        }
      ),
    ];
  };

let tests = ("Reach", pure_tests @ solve_tests);
