/* These are very simple tests to make sure we're not
   doing exponential blowup in the evaluator */

open Alcotest;
open Test_Evaluator_Prelude;
open Language;

let program = {hz|
let unique = (fun xs ->
  fold_left(xs, fun (seen, x) ->if mem(seen,x) then seen else seen @[x], []))in
let pivot_table = (fun (table, new_col, index, value) ->
  let indices = map(table, index) |> unique in
  let new_cols = map(table, new_col) |> unique in

  map(indices, fun idx ->
    (index=idx) ...
    (map(new_cols, fun col ->
      (label=col,
        value=filter(table, fun r -> index(r) == idx && new_col(r) == col)
        |>value)
    ) |> from_lvs)
  ))in



let results =
  pivot_table(
    [(a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3) ,(a=1,b=2,c=3)]
    @ [(a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3), (a=1,b=2,c=3) ,(a=1,b=2,c=3)],
    fun r -> r.a |> string_of_int,
    fun r -> r.b,
    fun r -> fold_left(r.c, int_plus, 0))
in

(results).`2`
|hz};

let tests = (
  "Evaluator.DynamicFeedback",
  [
    test_case(
      "dynamic in-editor feedback",
      `Slow,
      () => {
        let exp = parse_exp(program);
        let elaborated =
          Elaborator.elaborate(
            ~probe_unknowns=true,
            Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
            exp,
          )
          |> fst;
        let (result, state: EvaluatorState.t) =
          Evaluator.evaluate(~env=Builtins.env_init, elaborated);

        let dynamics = EvaluatorState.get_probes(state);
        print_endline(
          "Dynamics length: "
          ++ string_of_int(List.length(dynamics |> Id.Map.to_list)),
        );
        let dynamic_expressions: Id.Map.t(list(TermBase.exp_t)) =
          Id.Map.map(
            d => {
              open Language;
              // TODO If we can deal with the circular dependencies it would be great to keep the full closure and filter to the closure selector for the statics.
              let exps =
                List.map((c: Dynamics.Probe.Closure.t) => c.value, d);
              exps;
            },
            dynamics,
          );
        let _static_feedback =
          Statics.mk(
            ~dynamics=dynamic_expressions,
            CoreSettings.on,
            Builtins.ctx_init(Some(Int)),
            exp,
          );

        let expected_exp =
          parse_exp({hz|
          [(index=2, `1`=3).`2`]
          |hz});
        check(testable_exp(), "Result of execution", expected_exp, result);
      },
    ),
    test_case(
      "dynamic in-editor feedback",
      `Slow,
      () => {
        let exp =
          parse_exp(
            {|let unique = ^^fold(fun xs ->
  fold_left(xs, fun (seen, x) ->if mem(seen,x) then seen else seen @[x], []))in
let pivot_table = fun (table, new_col, index, value) ->
  let indices = map(table, index) |> unique in
  let new_cols = map(table, new_col) |> unique in

  map(indices, fun idx ->
    (index=idx) ...
    (map(new_cols, fun col ->
      (label=col,
        value=filter(table, fun r -> index(r) == idx && new_col(r) == col)
        |>value)
    ) |> from_lvs)
  )in

let table = [(a=1,b=2,c=3), (a=1,b=2,c=3)]in

let results =
  pivot_table(
    [(a=1,b=2,c=3), (a=1,b=2,c=3)],
    fun r -> r.a |> string_of_int,
    fun r -> r.b,
    fun r -> fold_left(r.c, int_plus, 0))
in
let results' =
  pivot_table(
    [(a=1,b=2,c=3), (a=1,b=2,c=3)],
    fun r -> r.a |> string_of_int,
    fun r -> r.b |> string_of_int,
    fun r -> fold_left(r.c, int_plus, 0))
in
results.`2`|},
          );
        let elaborated =
          Elaborator.elaborate(
            ~probe_unknowns=true,
            Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
            exp,
          )
          |> fst;
        let (result, state: EvaluatorState.t) =
          Evaluator.evaluate(~env=Builtins.env_init, elaborated);

        let dynamics = EvaluatorState.get_probes(state);
        print_endline(
          "Dynamics length: "
          ++ string_of_int(List.length(dynamics |> Id.Map.to_list)),
        );
        let dynamic_expressions: Id.Map.t(list(TermBase.exp_t)) =
          Id.Map.map(
            d => {
              open Language;
              // TODO If we can deal with the circular dependencies it would be great to keep the full closure and filter to the closure selector for the statics.
              let exps =
                List.map((c: Dynamics.Probe.Closure.t) => c.value, d);
              exps;
            },
            dynamics,
          );
        let _static_feedback =
          Statics.mk(
            ~dynamics=dynamic_expressions,
            CoreSettings.on,
            Builtins.ctx_init(Some(Int)),
            exp,
          );

        let expected_exp =
          parse_exp({hz|
          [(index=2, `1`=3).`2`]
          |hz});
        check(testable_exp(), "Result of execution", expected_exp, result);
      },
    ),
  ],
);
