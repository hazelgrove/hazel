/* These are very simple tests to make sure we're not
   doing exponential blowup in the evaluator */

open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Each f(i) calls f(i-1) twice, so inlining every callee at every level
   copies f0 into `go` 2^10 times. `render` is unbound, so the result is
   stuck holding `go`. */
let stuck_over_module = {|module M = {
  let f0 = fun x -> x + 1;
  let f1 = fun x -> f0(f0(x));
  let f2 = fun x -> f1(f1(x));
  let f3 = fun x -> f2(f2(x));
  let f4 = fun x -> f3(f3(x));
  let f5 = fun x -> f4(f4(x));
  let f6 = fun x -> f5(f5(x));
  let f7 = fun x -> f6(f6(x));
  let f8 = fun x -> f7(f7(x));
  let f9 = fun x -> f8(f8(x));
  let f10 = fun x -> f9(f9(x));
  let go = fun n -> if n == 0 then 0 else f10(go(n - 1))
} in
|};

let count_exps = (p: Exp.t => bool, e: Exp.t): int => {
  let n = ref(0);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          if (p(e)) {
            incr(n);
          };
          cont(e);
        },
      e,
    );
  n^;
};

let is_var = (x, e: Exp.t) =>
  switch (e.term) {
  | Var(y) => x == y
  | _ => false
  };

let check_shown_small = (msg, e: Exp.t) => {
  let size = count_exps(_ => true, e);
  if (size > 100) {
    fail(Printf.sprintf("%s: %d terms", msg, size));
  };
  check(
    bool,
    msg ++ " names a nested callee",
    true,
    count_exps(is_var("f9"), e) > 0,
  );
};

let tests = (
  "Evaluator.Performance",
  [
    test_case("lists don't accumulate from patterns", `Slow, () => {
      parse_and_evaluate_test(
        "100",
        {|let list_of_ones = fun n ->
            if n == 0 then [] else 1 :: list_of_ones(n-1)
          in

          let sum = fun l:[Int] ->
            case l
              | [] => 0
              | x::xs => x + sum(xs)
            end
          in

          let n = 100 in

          sum(list_of_ones(n))|},
      )
    }),
    test_case("stuck result doesn't inline callees at every level", `Quick, () =>
      check_shown_small(
        "result",
        parse_and_evaluate(stuck_over_module ++ "render(M.go)"),
      )
    ),
    test_case(
      "probed stuck value doesn't inline callees at every level",
      `Quick,
      () => {
        let (_, elaborated, _, targets) =
          parse_with_probes(
            stuck_over_module ++ "let stuck = render(M.go) in ^^probe(stuck)",
          );
        let (_, state) =
          Evaluator.evaluate(
            ~eval_info=EvalInfo.of_targets(targets),
            ~env=Builtins.env_init(),
            elaborated,
          );
        let values =
          Sample.Map.fold(
            (_, samples, acc) =>
              List.concat_map(
                (s: Sample.t) =>
                  List.filter_map(
                    (en: Sample.Env.entry) =>
                      switch (en.value) {
                      | Val(v) => Some(v)
                      | Opaque => None
                      },
                    s.env,
                  ),
                samples,
              )
              @ acc,
            EvaluatorState.get_probes(state),
            [],
          );
        check(int, "one captured value", 1, List.length(values));
        List.iter(check_shown_small("sample env value"), values);
      },
    ),
  ],
);
