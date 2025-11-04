/* These are very simple tests to make sure we're not
   doing exponential blowup in the evaluator */

open Alcotest;
open Test_Evaluator_Prelude;
open Language;

[@deriving show({with_path: false})]
type error =
  | NoError
  | StaticError(Info.error)
  | DynamicError(Info.error);

let testable_error =
  testable(Fmt.using(show_error, Fmt.string), (a: error, b: error) =>
    switch (a, b) {
    | (NoError, NoError) => true
    | (StaticError(e1), StaticError(e2)) => Info.equal_error(e1, e2)
    | (DynamicError(e1), DynamicError(e2)) => Info.equal_error(e1, e2)
    | _ => false
    }
  );
module FError =
  Grammar.Factory({
    type t = error;
    let default_value = (): error => {
      NoError;
    };
  });

let tests = (
  "Evaluator.DynamicFeedback",
  [
    test_case(
      "dynamic in-editor feedback",
      `Slow,
      () => {
        let program = {hazel|
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
|hazel};
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
          [(index=2, `1`=30).`2`]
          |hz});
        check(testable_exp(), "Result of execution", expected_exp, result);
      },
    ),
    test_case(
      "1 : ? : String",
      `Quick,
      () => {
        let exp: FError.exp =
          FError.(
            Exp.(
              asc(
                asc(
                  ~ann=
                    DynamicError(
                      Exp(
                        Common(
                          Inconsistent(
                            Test_Statics_Prelude.FTemp.Typ.(
                              Expectation({
                                ana: string(),
                                syn: int(),
                              })
                            ),
                          ),
                        ),
                      ),
                    ),
                  int(1),
                  Typ.unknown(Internal),
                ),
                Typ.string(),
              )
            )
          );
        let exp_id: Exp.t =
          Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh(), exp);
        let s =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp_id);
        let elaborated =
          Elaborator.elaborate(~probe_unknowns=true, s, exp_id) |> fst;
        let (_, state) =
          Evaluator.evaluate(~env=Builtins.env_init, elaborated);

        let dynamics = EvaluatorState.get_probes(state);

        let dynamic_expressions: Id.Map.t(list(TermBase.exp_t)) =
          Id.Map.map(
            d => {
              open Language;
              let exps =
                List.map((c: Dynamics.Probe.Closure.t) => c.value, d);
              exps;
            },
            dynamics,
          );

        let dynamic_static_feedback =
          Statics.mk(
            ~dynamics=dynamic_expressions,
            CoreSettings.on,
            Builtins.ctx_init(Some(Int)),
            exp_id,
          );

        let actual: FError.exp =
          Grammar.map_exp_annotation(
            id_tag => {
              let foo =
                StaticsBase.Map.lookup(IdTagged.IdTag.rep_id(id_tag), s);

              switch (Option.bind(foo, Info.error_of)) {
              | Some(e) => StaticError(e)
              | None =>
                switch (
                  StaticsBase.Map.lookup(
                    IdTagged.IdTag.rep_id(id_tag),
                    dynamic_static_feedback,
                  )
                ) {
                | Some(info) =>
                  switch (Info.error_of(info)) {
                  | Some(e) => DynamicError(e)
                  | None => NoError
                  }
                | None => NoError
                }
              };
            },
            exp_id,
          );

        check(
          Test_Statics_Prelude.annotated_exp'(testable_error),
          "Dynamic feedback",
          exp,
          actual,
        );
      },
    ),
  ],
);
