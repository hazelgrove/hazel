open Virtual_dom.Vdom;
open Node;
open Core;

let evaluate =
  Core_kernel.Memo.general(~cache_size_bound=1000, Evaluator.evaluate);

let convert_metrics = (font_metrics: FontMetrics.t): DHCode.font_metrics => {
  row_height: font_metrics.row_height,
  col_width: font_metrics.col_width,
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

let dhcode_view = (~font_metrics: FontMetrics.t) => {
  DHCode.view_tylr(
    ~selected_instance=None, //option((int, int)) // hole, hole_inst
    ~font_metrics=convert_metrics(font_metrics),
    ~settings=Settings.Evaluation.init,
  );
};

let res_view = (~font_metrics: FontMetrics.t, term, info_map): Node.t => {
  let result = get_result(Elaborator.uexp_elab(info_map, term));
  div(
    [Attr.classes(["result"])],
    switch (result) {
    | None => []
    | Some((result, _)) => [dhcode_view(~font_metrics, ~width=80, result)]
    },
  );
};
