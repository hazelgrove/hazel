open Alcotest;
open Haz3lcore;
open Language;

/* Ids are random, so an expression is found by its source text. */
let text = (e: Exp.t): string =>
  e
  |> ExpToSegment.exp_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
     )
  |> Printer.of_segment(~holes="?");

/* The evaluation count of every representative expression, by its text. */
let evaluations = (program: string): list((string, int)) => {
  let (info_map, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      Test_Evaluator_Prelude.parse_exp(program),
    );
  let (_, state) =
    Evaluator.evaluate(
      ~eval_info=
        EvalInfo.of_info_map(
          ~probe_all=false,
          ~targets=Sample.no_targets,
          info_map,
        ),
      ~env=Builtins.env_init,
      elab,
    );
  let counts = EvaluatorState.get_evaluations(state);
  Id.Map.fold(
    (id, info, acc) =>
      switch (info) {
      | Info.InfoExp(e) when Id.equal(id, Info.id_of(info)) => [
          (
            text(e.user_term),
            Option.value(Id.Map.find_opt(id, counts), ~default=0),
          ),
          ...acc,
        ]
      | _ => acc
      },
    info_map,
    [],
  );
};

let check_counts = (program: string, expected: list((string, int))) => {
  let counts = evaluations(program);
  List.iter(
    ((text, n)) =>
      switch (List.assoc_opt(text, counts)) {
      | Some(count) => check(int, text, n, count)
      | None =>
        fail(
          "no expression "
          ++ text
          ++ " among "
          ++ String.concat(", ", List.map(fst, counts)),
        )
      },
    expected,
  );
};

let test_once_per_call =
  test_case("evaluate: a function body is evaluated once per call", `Quick, () =>
    check_counts(
      "let f = fun n -> n + 1 in f(1) + f(2)",
      [("f(1) + f(2)", 1), ("f(1)", 1), ("n + 1", 2), ("n", 2)],
    )
  );

let test_unevaluated =
  test_case("evaluate: a branch not taken is not evaluated", `Quick, () =>
    check_counts(
      "if true then 1 + 1 else 2 + 2",
      [("1 + 1", 1), ("2 + 2", 0)],
    )
  );

/* A projection revisits the record's value under the ids of its literals. */
let test_values =
  test_case(
    "evaluate: a value is not evaluated, however often it is revisited",
    `Quick,
    () =>
    check_counts(
      "let g = [(a=1, b=8)] in g.b",
      [("g.b", 1), ("8", 0), ("(a=1, b=8)", 0)],
    )
  );

let test_recursion =
  test_case(
    "evaluate: a recursive body is evaluated once per unwinding", `Quick, () =>
    check_counts(
      "let f = fun n -> if n == 0 then 0 else f(n - 1) in f(3)",
      [("f(3)", 1), ("n == 0", 4), ("f(n - 1)", 3), ("n - 1", 3)],
    )
  );

let tests = (
  "Evaluator_Evaluations",
  [test_once_per_call, test_unevaluated, test_values, test_recursion],
);
