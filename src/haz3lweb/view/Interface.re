open Haz3lcore;

let evaluate = Core.Memo.general(~cache_size_bound=1000, Evaluator.evaluate);

let dhcode_view = (~font_metrics: FontMetrics.t) => {
  DHCode.view_tylr(
    ~selected_instance=None, //option((int, int)) // hole, hole_inst
    ~font_metrics,
    ~settings=Settings.Evaluation.init,
  );
};

let get_result =
    (d: Elaborator.ElaborationResult.t): option((DHExp.t, TestMap.t)) => {
  print_endline("get_result");
  switch (d) {
  | Elaborates(elab, _, _) =>
    switch (elab |> evaluate) {
    | (EvaluatorResult.BoxedValue(d), {test_map, _})
    | (Indet(d), {test_map, _}) =>
      print_endline("444444");
      Some((d, List.rev(test_map)));
    /*| exception _ =>
      print_endline("EXCEPTION THROWN IN GET_RESULT");
      None;*/
    }
  | _ =>
    print_endline("333333333");
    None;
  };
};

let evaluation_result = (map, term): option(DHExp.t) => {
  let elab = Haz3lcore.Elaborator.uexp_elab(map, term);
  print_endline(
    Sexplib.Sexp.to_string_hum(Elaborator.ElaborationResult.sexp_of_t(elab)),
  );
  switch (Haz3lcore.Elaborator.uexp_elab(map, term) |> get_result) {
  | None => None
  | Some((result, _)) => Some(result)
  };
};

type test_results = {
  test_map: TestMap.t,
  statuses: list(TestStatus.t),
  descriptions: list(string),
  total: int,
  passing: int,
  failing: int,
  unfinished: int,
};

let mk_results = (~descriptions=[], test_map: TestMap.t): test_results => {
  test_map,
  statuses: test_map |> List.map(r => r |> snd |> TestMap.joint_status),
  descriptions,
  total: TestMap.count(test_map),
  passing: TestMap.count_status(Pass, test_map),
  failing: TestMap.count_status(Fail, test_map),
  unfinished: TestMap.count_status(Indet, test_map),
};

let test_results = (~descriptions=[], map, term): option(test_results) => {
  switch (Haz3lcore.Elaborator.uexp_elab(map, term) |> get_result) {
  | None
  | Some((_, [])) => None
  | Some((_, test_map)) => Some(mk_results(~descriptions, test_map))
  };
};
