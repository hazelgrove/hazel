open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;

let show_term = (editor: Editor.t, _) =>
  editor.state.zipper
  |> Zipper.zip
  |> MakeTerm.go
  |> Term.UExp.show
  |> print_endline
  |> (_ => Event.Ignore);

let get_goal = (~font_metrics: FontMetrics.t, ~target_id, e) => {
  let rect = JSUtil.force_get_elem_by_id(target_id)##getBoundingClientRect;
  let goal_x = float_of_int(e##.clientX);
  let goal_y = float_of_int(e##.clientY);
  Measured.Point.{
    row: Float.to_int((goal_y -. rect##.top) /. font_metrics.row_height),
    col:
      Float.(
        to_int(round((goal_x -. rect##.left) /. font_metrics.col_width))
      ),
  };
};

let mousedown_handler =
    (~inject, ~font_metrics, ~target_id, ~additional_updates=[], e) => {
  let goal = get_goal(~font_metrics, ~target_id, e);
  Event.Many(
    List.map(inject, additional_updates)
    @ [
      inject(Update.Mousedown),
      inject(Update.PerformAction(Move(Goal(goal)))),
    ],
  );
};

let deco = (~zipper, ~map, ~segment, ~font_metrics, ~show_backpack_targets) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = show_backpack_targets;
    });
  Deco.all(zipper, segment);
};

let mousedown_overlay = (~inject, ~font_metrics, ~target_id) =>
  div(
    Attr.[
      id("mousedown-overlay"),
      on_mouseup(_ => inject(Update.Mouseup)),
      on_mousemove(e => {
        let goal = get_goal(~font_metrics, ~target_id, e);
        inject(Update.PerformAction(Select(Goal(goal))));
      }),
    ],
    [],
  );

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
      ? deco(
          ~zipper,
          ~map=measured,
          ~segment,
          ~font_metrics,
          ~show_backpack_targets,
        )
      : [];
  div(
    [Attr.id(id), Attr.class_("code-container")],
    [code_view] @ deco_view @ overlays,
  );
};

let cell_view =
    (
      ~cell_caption: list(Node.t)=[],
      ~cell_chapter: list(Node.t)=[],
      ~result_bar: list(Node.t)=[],
      ~settings: Model.settings,
      ~inject: Update.t => 'a,
      ~font_metrics,
      ~selected,
      ~mousedown,
      ~show_backpack_targets,
      ~show_code=true,
      ~overlays=[],
      idx,
      editor: Editor.t,
    ) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let cell_chapter_view = div([clss(["cell-chapter"])], cell_chapter);
  let code_container_id = "code-container-" ++ string_of_int(idx);
  let code_view =
    code_container(
      ~id=code_container_id,
      ~font_metrics,
      ~unselected,
      ~settings,
      ~overlays,
      ~show_backpack_targets,
      ~show_deco=selected == idx,
      ~measured=editor.state.meta.measured,
      zipper,
    );
  let mousedown_overlay =
    selected == idx && mousedown
      ? [
        mousedown_overlay(
          ~inject,
          ~font_metrics,
          ~target_id=code_container_id,
        ),
      ]
      : [];
  let cell_caption_view =
    div(
      [clss(["cell-caption"]), Attr.on_click(show_term(editor))],
      cell_caption,
    );
  div(
    [clss(["cell-container"])],
    [cell_chapter_view]
    @ [
      div(
        [
          Attr.classes(["cell"] @ (selected == idx ? ["selected"] : [])),
          Attr.on_mousedown(
            mousedown_handler(
              ~inject,
              ~font_metrics,
              ~target_id=code_container_id,
              ~additional_updates=[Update.SwitchEditor(idx)],
            ),
          ),
        ],
        [cell_caption_view]
        @ (show_code ? mousedown_overlay @ [code_view] : []),
      ),
    ]
    @ result_bar,
  );
};
