open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.Fixpoints",
  [
    /* The binder's ascription is how the fixpoint re-checks at its own type,
       so the copies an unroll makes keep it. */
    test_case(
      "An unrolled fixpoint keeps its binder's ascription",
      `Quick,
      () => {
        let bare = ref(0);
        let rec go = (n, e: Language.Exp.t) => {
          let _ =
            Language.Exp.map_term(
              ~f_exp=
                (continue, e: Language.Exp.t) => {
                  switch (e.term) {
                  | FixF({term: Var(_), _}, _, _) => bare := bare^ + 1
                  | _ => ()
                  };
                  continue(e);
                },
              e,
            );
          switch (n < 10 ? single_step(e) : None) {
          | Some(e) => go(n + 1, e)
          | None => ()
          };
        };
        go(0, elaborate(parse_exp("let f = fun _ -> f in f(1)")));
        check(int, "fixpoints with a bare binder", 0, bare^);
      },
    ),
    /* A recursive call runs under the function's own environment, not the
       caller's: the inner `y = 100` must not reach the body's `y`. */
    test_case(
      "A recursive call sees the function's environment",
      `Quick,
      () => {
        let program = "let y = 1 in let f = fun x -> if x == 0 then y else (let y = 100 in f(x - 1)) in f(1)";
        parse_and_evaluate_test(~msg="evaluator", "1", program);
        check(
          step_limited,
          "stepper",
          LimitedCompleted((parse_exp("1"), Language.EvaluatorState.empty)),
          full_small_step_reduction(elaborate(parse_exp(program))),
        );
      },
    ),
    test_case("Inconsistent type in fixpoint pattern", `Quick, () =>
      parse_and_evaluate_test("[] : ()", {|fix () -> []|})
    ),
    test_case("Fixpoint with boolean short circuiting", `Quick, () =>
      parse_and_evaluate_test("true", {|fix f -> true || f|})
    ),
    test_case("Fixpoint with function", `Quick, () =>
      parse_and_evaluate_test(
        "3",
        {|(fix f -> fun i -> if i > 2 then i else f(i+1))(1)|},
      )
    ),
    test_case("Mutual recursion", `Quick, () =>
      parse_and_evaluate_test(
        "false",
        {|let (even, odd) = (fix (even, odd) -> (fun n -> if n == 0 then true else odd(n - 1),
fun n -> if n == 0 then false else even(n - 1))) in even(1)|},
      )
    ),
    test_case("Fixpoint with wildcard unit type", `Quick, () =>
      parse_and_evaluate_test("()", {|fix _ : () -> test B end|})
    ),
    test_case("Fixpoint pattern ascription", `Quick, () =>
      parse_and_evaluate_test(
        "() : Bool",
        {|fix (_ : Bool) -> test false end|},
      )
    ),
    test_case("Fixpoint with type ascription", `Quick, () =>
      parse_and_evaluate_test("() : Int", {|fix 0 -> ()|})
    ),
    test_case("Substitution", `Quick, () =>
      parse_and_evaluate_test(
        "1",
        {|let t =  1 in
          let go =
            fun i ->
          if i > 1 then t else go(i+1) in
          go(0)|},
      )
    ),
    test_case("Mutual recursion substitution", `Quick, () =>
      parse_and_evaluate_test(
        "1",
        {|let t =  1 in
          let (go, go') =
            (fun i ->
          if i > 3 then t else go'(i+1),fun i -> if i > 3 then t else go(i+1) ) in
          go(0)|},
      )
    ),
    test_case("Fixpoint that evaluates to the correct form", `Quick, () =>
      parse_and_evaluate_test("(1, 2)", {|let x = (1, 2) in fix f -> x|})
    ),
  ],
);
