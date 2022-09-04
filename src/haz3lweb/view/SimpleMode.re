open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

let test_view =
    (~title, ~inject, ~font_metrics, ~test_results: Interface.test_results): t =>
  div(
    ~attr=clss(["panel", "test-panel"]),
    [
      TestView.view_of_main_title_bar(title),
      TestView.test_reports_view(~inject, ~font_metrics, ~test_results),
      TestView.test_summary(~inject, ~test_results),
    ],
  );

let res_view = (~font_metrics: FontMetrics.t, eval_result): Node.t =>
  div(
    ~attr=Attr.classes(["result"]),
    [Interface.dhcode_view(~font_metrics, ~width=80, eval_result)],
  );

let single_editor_semantics_views =
    (~inject, ~font_metrics, ~settings: Model.settings, ~index, ~unselected) => {
  let (term, _) = MakeTerm.go(unselected);
  let map = Statics.mk_map(term);
  let test_results =
    settings.dynamics ? Interface.test_results(map, term) : None;
  let eval_result =
    settings.dynamics ? Interface.evaluation_result(map, term) : None;
  [
    div(
      ~attr=clss(["bottom-bar"]),
      [CursorInspector.view(~inject, ~settings, index, map)]
      @ (
        switch (eval_result) {
        | _ when !settings.dynamics => []
        | None => []
        | Some(eval_result) => [res_view(~font_metrics, eval_result)]
        }
      ),
    ),
  ]
  @ (
    switch (test_results) {
    | _ when !settings.dynamics => []
    | None => []
    | Some(test_results) => [
        test_view(~title="Tests", ~inject, ~font_metrics, ~test_results),
      ]
    }
  );
};

let code_container =
    (
      ~font_metrics,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco,
      ~overlays=[],
      ~id,
      ~measured,
      zipper,
    ) => {
  let segment = Zipper.zip(zipper);
  let code_view =
    Code.view(~font_metrics, ~segment, ~unselected, ~map=measured, ~settings);
  let deco_view =
    show_deco
      ? {
        module Deco =
          Deco.Deco({
            let font_metrics = font_metrics;
            let map = measured;
            let show_backpack_targets = show_backpack_targets;
            let (term, terms) = MakeTerm.go(unselected);
            let info_map = Statics.mk_map(term);
            let term_ranges = TermRanges.mk(unselected);
            let tiles = TileMap.mk(unselected);
          });
        Deco.all(zipper, unselected);
      }
      : [];
  div(
    ~attr=Attr.many([Attr.id(id), Attr.class_("code-container")]),
    [code_view] @ deco_view @ overlays,
  );
};

let cell_result_view = (~font_metrics, unselected) => {
  let (term, _) = MakeTerm.go(unselected);
  let map = Statics.mk_map(term);
  switch (Interface.evaluation_result(map, term)) {
  | None => []
  | Some(eval_result) => [
      div(
        ~attr=clss(["cell-result"]),
        [res_view(~font_metrics, eval_result)],
      ),
    ]
  };
};

let view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
      ~measured: Measured.t,
    ) => {
  let unselected = Zipper.unselect_and_zip(zipper);
  let code_id = "code-container";
  let code_view =
    code_container(
      ~id=code_id,
      ~font_metrics,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco=true,
      ~measured,
      zipper,
    );
  let result_view =
    !settings.dynamics ? [] : cell_result_view(~font_metrics, unselected);
  let cell_view =
    div(
      ~attr=clss(["cell-container"]),
      [
        Cell.view(
          ~inject,
          ~font_metrics,
          ~clss=["single"],
          ~mousedown,
          ~selected=true,
          ~show_code=true,
          ~code_id,
          code_view,
        ),
      ]
      @ result_view,
    );
  let semantics_views =
    settings.statics
      ? single_editor_semantics_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~index=Indicated.index(zipper),
          ~unselected,
        )
      : [];
  div(~attr=clss(["editor", "single"]), [cell_view] @ semantics_views);
};
