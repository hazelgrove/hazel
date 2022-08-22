open OptUtil.Syntax;
open Core;

let evaluate =
  Core_kernel.Memo.general(~cache_size_bound=1000, Evaluator.evaluate);

let convert_metrics = (font_metrics: FontMetrics.t): DHCode.font_metrics => {
  row_height: font_metrics.row_height,
  col_width: font_metrics.col_width,
};

let dhcode_view = (~font_metrics: FontMetrics.t) => {
  DHCode.view_tylr(
    ~selected_instance=None, //option((int, int)) // hole, hole_inst
    ~font_metrics=convert_metrics(font_metrics),
    ~settings=Settings.Evaluation.init,
  );
};

let get_result =
    (d: Elaborator_Exp.ElaborationResult.t): option((DHExp.t, TestMap.t)) => {
  switch (d) {
  | Elaborates(elab, _, _) =>
    switch (elab |> evaluate) {
    | (Evaluator.BoxedValue(d), {test_map, _})
    | (Indet(d), {test_map, _}) => Some((d, List.rev(test_map)))
    | exception _ => None
    }
  | _ => None
  };
};

let evaulation_result = (map, term): option(DHExp.t) =>
  switch (Elaborator.uexp_elab(map, term) |> get_result) {
  | None => None
  | Some((result, _)) => Some(result)
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
  switch (Elaborator.uexp_elab(map, term) |> get_result) {
  | None
  | Some((_, [])) => None
  | Some((_, test_map)) => Some(mk_results(~descriptions, test_map))
  };
};

type semantics_package = {
  term: Term.UExp.t,
  ty: Typ.t,
  free: Ctx.co,
  map: Statics.map,
  elab: Elaborator_Exp.ElaborationResult.t,
  result: DHExp.t,
  test_map: TestMap.t,
  hii: HoleInstanceInfo.t,
};

let semantics_of_zipper = (zipper: Zipper.t): option(semantics_package) => {
  let segment = Zipper.unselect_and_zip(zipper);
  let term = MakeTerm.go(segment);
  let (ty, free, map) = Statics.mk_map(term);
  let elab = Elaborator.uexp_elab(map, term);
  let+ (result, test_map) = get_result(elab);
  let (_d_renumbered, hii) =
    Elaborator_Exp.renumber([], HoleInstanceInfo.empty, result);
  {term, ty, free, map, elab, result, test_map, hii};
};

let cursor_dynamics = (zipper: Zipper.t) => {
  let* index = Indicated.index(zipper);
  let segment = Zipper.unselect_and_zip(zipper);
  let term = MakeTerm.go(segment);
  let (_, _, map) = Statics.mk_map(term);
  let elab = Elaborator.uexp_elab(~probe=Some(index), map, term);
  let _ =
    switch (elab) {
    | Elaborates(dhexp, _, _) =>
      print_endline("STARY ELAB");
      print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(dhexp)));
      print_endline("END ELAB");
    | _ => print_endline("didn't elaborate")
    };
  let* (_result, test_map) = get_result(elab);
  print_endline(Sexplib.Sexp.to_string_hum(TestMap.sexp_of_t(test_map)));
  /*let (_d_renumbered, hii) =
    Elaborator_Exp.renumber([], HoleInstanceInfo.empty, result);*/
  let+ instances = TestMap.lookup(-666, test_map);
  print_endline(
    Sexplib.Sexp.to_string_hum(
      Sexplib.Std.sexp_of_list(
        TestMap.sexp_of_test_instance_report,
        instances,
      ),
    ),
  );
  instances;
};
