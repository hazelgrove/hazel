open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;

let semantics_views =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      ~index,
      ~unselected: Segment.t,
      ~zipper: Zipper.t,
    ) => {
  let term = MakeTerm.go(unselected);
  let (_, _, map) = Statics.mk_map(term);
  let statics_view = [CursorInspector.view(~inject, ~settings, index, map)];
  let test_results =
    settings.dynamics ? Interface.test_results(map, term) : None;
  let eval_result =
    settings.dynamics ? Interface.evaulation_result(map, term) : None;
  //HACK,TODO(andrew): remove duplicates due to multiple instances due to... cases?
  let instances =
    settings.dynamics
      ? Interface.cursor_dynamics(zipper)
        |> Option.map(List.sort_uniq(compare))
      : None;
  let instance_result_view =
    LiveInspector.instance_result_view(
      ~settings,
      ~inject,
      ~font_metrics,
      eval_result,
      instances,
    );
  let instance_environment_view =
    LiveInspector.instance_environment_view(
      ~inject,
      ~font_metrics,
      ~settings,
      instances,
    );
  let test_view =
    switch (test_results) {
    | _ when !settings.dynamics => []
    | None => []
    | Some(test_results) => [
        TestView.view(~title="Tests", ~inject, ~font_metrics, ~test_results),
      ]
    };
  [div([clss(["bottom-bar"])], statics_view @ instance_result_view)]
  @ instance_environment_view
  @ test_view;
};

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

let deco = (~zipper, ~map, ~segment, ~font_metrics, ~show_backpack_targets) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = show_backpack_targets;
    });
  Deco.all(zipper, segment);
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

let view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
      ~measured: Measured.t,
    )
    : Node.t => {
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
  let semantics_views =
    settings.statics
      ? semantics_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~index=Indicated.index(zipper),
          ~unselected,
          ~zipper,
        )
      : [];
  let mousedown_overlay =
    mousedown
      ? [mousedown_overlay(~inject, ~font_metrics, ~target_id=code_id)] : [];
  div(
    [
      clss(["editor", "single"]),
      //TODO(andrew): re-enable
      /*Attr.on_mousedown(e =>
          mousedown_handler(~inject, ~font_metrics, ~target_id=code_id, e)
        ),*/
    ],
    [code_view] @ semantics_views @ mousedown_overlay,
  );
};
