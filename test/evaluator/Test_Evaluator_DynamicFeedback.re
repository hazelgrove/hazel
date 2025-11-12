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

/**
 * Helper function to extract dynamic expressions from probe closures.
 * This logic is shared between multiple test cases.
 */
let create_dynamic_expressions =
    (dynamics: Id.Map.t(list(Dynamics.Probe.Closure.t))) =>
  Id.Map.map(
    (closures: list(Dynamics.Probe.Closure.t)): DynamicStatics.Map.entry => {
      Language.{
        exps: List.map((c: Dynamics.Probe.Closure.t) => c.value, closures),
        ty_envs:
          List.map((c: Dynamics.Probe.Closure.t) => c.ty_env, closures),
      }
    },
    dynamics,
  );

/**
 * Maps static and dynamic error information to error annotations.
 * Simplifies the nested switch logic with pattern matching.
 */
let map_error_annotation = (static_info, dynamic_info) => {
  let static_error = Option.bind(static_info, Info.error_of);
  let dynamic_error = Option.bind(dynamic_info, Info.error_of);

  switch (static_error, dynamic_error) {
  | (Some(e), _) => StaticError(e)
  | (None, Some(e)) => DynamicError(e)
  | (None, None) => NoError
  };
};

/**
 * Reusable test function for dynamic feedback validation.
 * Takes an expected expression with error annotations and verifies
 * that the dynamic feedback system correctly identifies errors.
 */
let test_dynamic_feedback = (~test_name=?, expected_exp: FError.exp) => {
  // Create expression with fresh IDs for static analysis
  let exp_with_ids: Exp.t =
    Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh(), expected_exp);

  let test_name =
    Util.OptUtil.get(
      () => {
        Haz3lcore.(
          Printer.of_segment(
            ~holes="?",
            ExpToSegment.exp_to_segment(
              ~settings=
                ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
              exp_with_ids,
            ),
          )
        )
      },
      test_name,
    );

  // Perform initial static analysis
  let initial_statics =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp_with_ids);

  // Elaborate the expression with unknown type probing enabled
  let elaborated_exp =
    Elaborator.elaborate(~probe_unknowns=true, initial_statics, exp_with_ids)
    |> fst;

  // Evaluate the elaborated expression to collect dynamic information
  let (_, evaluation_state) =
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      ~ty_env=Environment.empty,
      elaborated_exp,
    );

  // Extract probe data from the evaluation state
  let probe_data = EvaluatorState.get_probes(evaluation_state);

  // Convert probe closures to dynamic expressions for static re-analysis
  let dynamic_expressions = create_dynamic_expressions(probe_data);

  // Re-run static analysis with dynamic information
  let dynamic_statics =
    Statics.mk(
      ~dynamics=dynamic_expressions,
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      exp_with_ids,
    );

  // Map the expression to annotate errors based on static and dynamic feedback
  let actual_exp: FError.exp =
    Grammar.map_exp_annotation(
      id_tag => {
        let static_info =
          StaticsBase.Map.lookup(
            IdTagged.IdTag.rep_id(id_tag),
            initial_statics,
          );
        let dynamic_info =
          StaticsBase.Map.lookup(
            IdTagged.IdTag.rep_id(id_tag),
            dynamic_statics,
          );

        map_error_annotation(static_info, dynamic_info);
      },
      exp_with_ids,
    );

  // Verify that the actual error annotations match expectations
  check(
    Test_Statics_Prelude.annotated_exp'(testable_error),
    test_name,
    expected_exp,
    actual_exp,
  );
};
let inconsistent_exp: Info.error_inconsistent => Info.error =
  e => Exp(Common(Inconsistent(e)));

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
          Evaluator.evaluate(
            ~env=Builtins.env_init,
            ~ty_env=Environment.empty,
            elaborated,
          );

        let dynamics = EvaluatorState.get_probes(state);

        // Convert probe closures to dynamic expressions for static re-analysis
        let dynamic_expressions = create_dynamic_expressions(dynamics);
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
      "Type flows through unknown ascription dynamically",
      `Quick,
      () => {
        // Create expected expression with dynamic error annotation
        // This tests that int(1) : ? : String correctly identifies
        // the type inconsistency when ? is resolved to int but expected as string
        let expected_exp: FError.exp =
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

        test_dynamic_feedback(expected_exp);
      },
    ),
    test_case(
      "Conditional uses runtime type information",
      `Quick,
      () => {
        open FError;
        open Exp;

        test_dynamic_feedback(
          bin_op(
            String(Concat),
            if_(
              bool(true),
              asc(
                ~ann=
                  DynamicError(
                    inconsistent_exp(
                      Test_Statics_Prelude.FTemp.Typ.(
                        Expectation({
                          ana: string(),
                          syn: int(),
                        })
                      ),
                    ),
                  ),
                int(1),
                Typ.unknown(Internal),
              ),
              asc(string("World"), Typ.unknown(Internal)),
            ),
            string("World"),
          ),
        );
        test_dynamic_feedback(
          bin_op(
            String(Concat),
            if_(
              bool(false),
              asc(int(1), Typ.unknown(Internal)),
              asc(string("Hello"), Typ.unknown(Internal)),
            ),
            string("World"),
          ),
        );
      },
    ),
    test_case(
      "Unannotated lambda applied to string causes dynamic error",
      `Quick,
      () => {
        open FError;
        open Exp;
        let exp: FError.exp =
          ap(
            Forward,
            fn(
              Pat.var("y"),
              bin_op(
                Int(Plus),
                var(
                  ~ann=
                    DynamicError(
                      inconsistent_exp(
                        Test_Statics_Prelude.FTemp.Typ.(
                          Expectation({
                            ana: int(),
                            syn: string(),
                          })
                        ),
                      ),
                    ),
                  "y",
                ),
                int(1),
              ),
            ),
            string(""),
          );
        test_dynamic_feedback(exp);
      },
    ),
    test_case(
      "Unannotated lambda called with inconsistent types gives no feedback",
      `Quick,
      () => {
        open FError;
        open Exp;
        let exp: FError.exp =
          let_(
            Pat.var("f"),
            fn(
              Pat.var("x"),
              list_concat(var("x"), list_lit([string("")])),
            ),
            seq(
              ap(Forward, var("f"), int(1)),
              ap(Forward, var("f"), float(2.0)),
            ),
          );
        test_dynamic_feedback(exp);
      },
    ),
    test_case(
      "Polymorphism",
      `Quick,
      () => {
        [@warning "-21"]
        open FError;
        open Exp;

        Alcotest.skip(); // TODO We need to figure out how to handle this
        // (typfun a -> fun x : a -> x)@<String>("")
        let exp: FError.exp =
          ap(
            Forward,
            typ_ap(
              typ_fun(
                TPat.var("a"),
                fn(Pat.(asc(Pat.var("x"), Typ.var("a"))), var("x")),
                None,
              ),
              Typ.string(),
            ),
            string(""),
          );
        test_dynamic_feedback(exp);
      },
    ),
  ],
);
