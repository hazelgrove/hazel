open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

// TODO move into a module
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

let mk_results = (r: ProgramResult.t): (DHExp.t, Interface.test_results) => {
  let eval_result = r |> ProgramResult.get_dhexp;
  let test_results =
    r
    |> ProgramResult.get_state
    |> EvaluatorState.get_tests
    |> Interface.mk_results;
  (eval_result, test_results);
};

let get_async_results =
    (res: option(ModelResult.t)): option((DHExp.t, Interface.test_results)) =>
  res
  |> Option.map(res =>
       res
       |> ModelResult.get_current_ok
       |> Option.value(~default=ModelResult.get_previous(res))
     )
  |> Option.map(mk_results);

let get_sync_results = (map, term) =>
  Interface.get_result(map, term) |> mk_results |> Option.some;

let single_editor_semantics_views =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      ~index,
      ~unselected,
      //~simple_result: option(ModelResult.simple),
      ~info_map: Statics.map,
    ) => {
  [
    div(
      ~attr=clss(["bottom-bar"]),
      [CursorInspector.view(~inject, ~settings, index, info_map)],
    ),
    // TODO
    // @ (
    //   switch (results) {
    //   | _ when !settings.dynamics => []
    //   | None => []
    //   | Some((_, test_results)) => [
    //       test_view(~title="Tests", ~inject, ~font_metrics, ~test_results),
    //     ]
    //   }
    // );
  ];
};

let view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~editor: Editor.t,
      ~settings: Model.settings,
      ~simple_result: option(ModelResult.simple),
    ) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  let code_id = "code-container";
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
      simple_result,
    );
  let semantics_views =
    settings.statics
      ? single_editor_semantics_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~index=Indicated.index(zipper),
          ~unselected,
          // ~simple_result,
          ~info_map,
        )
      : [];
  div(~attr=clss(["editor", "single"]), [editor_view] @ semantics_views);
};
