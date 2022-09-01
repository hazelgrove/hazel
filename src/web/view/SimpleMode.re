open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;

let test_view =
    (~title, ~inject, ~font_metrics, ~test_results: Interface.test_results): t =>
  div(
    [clss(["panel", "test-panel"])],
    [
      TestView.view_of_main_title_bar(title),
      TestView.test_reports_view(~inject, ~font_metrics, ~test_results),
      TestView.test_summary(~inject, ~test_results),
    ],
  );

let res_view = (~font_metrics: FontMetrics.t, eval_result): Node.t =>
  div(
    [Attr.classes(["result"])],
    [Interface.dhcode_view(~font_metrics, ~width=80, eval_result)],
  );

let single_editor_semantics_views =
    (~inject, ~font_metrics, ~settings: Model.settings, ~index, ~unselected) => {
  let term = MakeTerm.go(unselected);
  let (_, _, map) = Statics.mk_map(term);
  let test_results =
    settings.dynamics ? Interface.test_results(map, term) : None;
  let eval_result =
    settings.dynamics ? Interface.evaluation_result(map, term) : None;
  [
    div(
      [clss(["bottom-bar"])],
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

let evaluate = (editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let segment = Core.Zipper.unselect_and_zip(zipper);
  let term = Core.MakeTerm.go(segment);
  let (_, _, info_map) = Core.Statics.mk_map(term);
  Interface.evaluation_result(info_map, term);
};

let result_view = (~font_metrics, result: option(DHExp.t)) => {
  [
    div(
      [clss(["cell-result"])],
      switch (result) {
      | None => [text("No result.")]
      | Some(dhexp) => [
          div(
            [clss(["cell-result-ok"])],
            [
              div([clss(["equivalence"])], [text("≡")]),
              res_view(~font_metrics, dhexp),
            ],
          ),
        ]
      },
    ),
  ];
};

let view =
    (
      ~inject,
      ~font_metrics,
      ~selected,
      ~show_backpack_targets,
      ~mousedown,
      ~editor: Editor.t,
      ~settings: Model.settings,
    )
    // ~measured: Measured.t,
    : Node.t => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let code_id = "code-container";
  let result = evaluate(editor);
  let result_view = result_view(~font_metrics, result);
  let cell_view =
    CodeCell.cell_view(
      ~cell_caption=[],
      ~result_bar=result_view,
      ~settings,
      ~inject,
      ~font_metrics,
      ~selected,
      ~mousedown,
      ~show_backpack_targets,
      0,
      editor,
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
  let mousedown_overlay =
    mousedown
      ? [
        CodeCell.mousedown_overlay(
          ~inject,
          ~font_metrics,
          ~target_id=code_id,
        ),
      ]
      : [];
  div(
    [
      clss(["editor", "single"]),
      Attr.on_mousedown(e =>
        CodeCell.mousedown_handler(
          ~inject,
          ~font_metrics,
          ~target_id=code_id,
          e,
        )
      ),
    ],
    [cell_view] @ semantics_views @ mousedown_overlay,
  );
};
