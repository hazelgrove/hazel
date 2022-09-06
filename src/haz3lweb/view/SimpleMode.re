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
    [
      DHCode.view_tylr(
        ~settings=Settings.Evaluation.init,
        ~selected_hole_instance=None,
        ~font_metrics,
        ~width=80,
        eval_result,
      ),
    ],
  );

let single_editor_semantics_views =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      ~index,
      ~unselected,
      ~res,
    ) => {
  let (term, _) = MakeTerm.go(unselected);
  let map = Statics.mk_map(term);
  let results = settings.dynamics ? get_results(res) : None;
  [
    div(
      ~attr=clss(["bottom-bar"]),
      [CursorInspector.view(~inject, ~settings, index, map)]
      @ (
        switch (results) {
        | _ when !settings.dynamics => []
        | None => []
        | Some((eval_result, _)) => [res_view(~font_metrics, eval_result)]
        }
      ),
    ),
  ]
  @ (
    switch (results) {
    | _ when !settings.dynamics => []
    | None => []
    | Some((_, test_results)) => [
        test_view(~title="Tests", ~inject, ~font_metrics, ~test_results),
      ]
    }
  );
};

let view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~editor: Editor.t,
      ~settings: Model.settings,
      ~res: option(ModelResult.t),
    ) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  let code_id = "code-container";
  let simple_result = ModelResult.get_simple(res);
  let result_view =
    !settings.dynamics ? [] : Cell.result_view(~font_metrics, simple_result);
  let editor_view =
    Cell.editor_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~clss=["single"],
      ~selected=true,
      ~mousedown,
      ~code_id,
      ~settings,
      ~info_map,
      editor,
    );
  let cell_view =
    div(~attr=clss(["cell-container"]), [editor_view] @ result_view);
  let semantics_views =
    settings.statics
      ? single_editor_semantics_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~index=Indicated.index(zipper),
          ~unselected,
          ~res,
        )
      : [];
  div(~attr=clss(["editor", "single"]), [cell_view] @ semantics_views);
};
