open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

// TODO move into a module
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
