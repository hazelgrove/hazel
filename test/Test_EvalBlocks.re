open Alcotest;
open Language;

let blocks = (program: string): list(Blocks.t) => {
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
  EvalBlocks.of_evaluations(
    ~info_map,
    EvaluatorState.get_evaluations(state),
  );
};

let paint = testable(Blocks.pp_paint, Blocks.equal_paint);
let shapes = list(pair(int, paint));

/* (level, paint) of every block in a fixed order: ids are random. */
let shape = (bs: list(Blocks.t)): list((int, Blocks.paint)) =>
  bs
  |> List.map((b: Blocks.t) => (b.level, b.paint))
  |> List.sort(((l1, p1), (l2, p2)) =>
       switch (Int.compare(l1, l2)) {
       | 0 => String.compare(Blocks.paint_name(p1), Blocks.paint_name(p2))
       | c => c
       }
     );

/* (level, heat) */
let check_shape = (program: string, expected: list((int, int))) =>
  check(
    shapes,
    program,
    List.map(((l, h)) => (l, Blocks.Heat(h)), expected),
    shape(blocks(program)),
  );

let test_once =
  test_case(
    "of_evaluations: a program evaluated once is one coolest block", `Quick, () =>
    check_shape("1 + 2", [(0, 0)])
  );

let test_not_taken =
  test_case(
    "of_evaluations: a branch not taken belongs to the block around it",
    `Quick,
    () =>
    check_shape("if true then 1 else 2", [(0, 0)])
  );

let test_hottest =
  test_case(
    "of_evaluations: the most evaluated term is the hottest block", `Quick, () =>
    check_shape(
      "let f = fun n -> n + 1 in f(1) + f(2) + f(3)",
      [(0, 0), (1, Blocks.heat_colors - 1)],
    )
  );

/* f's body runs four times, g's twice: heat is logarithmic, so twice out
   of four is midway. */
let test_logarithmic =
  test_case("of_evaluations: heat is logarithmic in the count", `Quick, () =>
    check_shape(
      "let f = fun n -> n + 1 in let g = fun m -> f(m) + f(m) in g(1) + g(2)",
      [(0, 0), (1, 2), (1, 5)],
    )
  );

let tests = (
  "EvalBlocks",
  [test_once, test_not_taken, test_hottest, test_logarithmic],
);
