open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

type state = (Id.t, Editor.t);

// let res_view = (~font_metrics: FontMetrics.t, eval_result): Node.t =>
//   div(
//     ~attr=Attr.classes(["result"]),
//     [
//       DHCode.view_tylr(
//         ~settings=Settings.Evaluation.init,
//         ~selected_hole_instance=None,
//         ~font_metrics,
//         ~width=80,
//         eval_result,
//       ),
//     ],
//   );

// let mk_results = (r: ProgramResult.t): (DHExp.t, Interface.test_results) => {
//   let eval_result = r |> ProgramResult.get_dhexp;
//   let test_results =
//     r
//     |> ProgramResult.get_state
//     |> EvaluatorState.get_tests
//     |> Interface.mk_results;
//   (eval_result, test_results);
// };

let view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~editor: Editor.t,
      ~settings: Model.settings,
      ~result: ModelResult.simple,
    ) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  let code_id = "code-container";
  let editor_view =
    Cell.editor_with_result_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~clss=["single"],
      ~selected=true,
      ~mousedown,
      ~code_id,
      ~settings,
      ~info_map,
      ~result,
      editor,
    );
  let ci_view =
    settings.statics
      ? [CursorInspector.view(~inject, ~settings, zipper, info_map)] : [];
  let bottom_bar = [div(~attr=Attr.class_("bottom-bar"), ci_view)];

  div(~attr=clss(["editor", "single"]), [editor_view] @ bottom_bar);
};
