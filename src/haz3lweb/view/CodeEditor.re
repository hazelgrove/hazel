open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
type editor_id = string;

type event =
  | MouseDown(Measured.Point.t)
  | DragTo(Measured.Point.t)
  | MouseUp
  | DoubleClick
  | TripleClick
  | JumpToBindingSiteOf(Measured.Point.t)
  | StepSelected(int);

let get_goal =
    (
      ~font_metrics: FontMetrics.t,
      text_box: Js.t(Dom_html.element),
      e: Js.t(Dom_html.mouseEvent),
    ) => {
  let rect = text_box##getBoundingClientRect;
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

let mousedown_overlay = (~inject, ~font_metrics) =>
  Node.div(
    ~attr=
      Attr.many(
        Attr.[
          id("mousedown-overlay"),
          on_mouseup(_ => inject(MouseUp)),
          on_mousemove(e => {
            let mouse_handler =
              e##.target |> Js.Opt.get(_, _ => failwith("no target"));
            let text_box =
              JsUtil.get_child_with_class(
                mouse_handler##.parentNode
                |> Js.Opt.get(_, _ => failwith(""))
                |> Js.Unsafe.coerce,
                "code-container",
              )
              |> Option.get;
            let goal = get_goal(~font_metrics, text_box, e);
            inject(DragTo(goal));
          }),
        ],
      ),
    [],
  );

let mousedown_handler = (~inject: event => 'a, ~font_metrics, evt) => {
  let goal =
    get_goal(
      ~font_metrics,
      evt##.currentTarget
      |> Js.Opt.get(_, _ => failwith(""))
      |> JsUtil.get_child_with_class(_, "code-container")
      |> Option.get,
      evt,
    );
  switch (JsUtil.ctrl_held(evt), JsUtil.num_clicks(evt)) {
  | (true, _) => inject(JumpToBindingSiteOf(goal))
  | (false, 1) => inject(MouseDown(goal))
  | (false, 2) => inject(DoubleClick)
  | (false, 3 | _) => inject(TripleClick)
  };
};

let test_status_icon_view =
    (~font_metrics, insts, ms: Measured.Shards.t): option(Node.t) =>
  switch (ms) {
  | [(_, {origin: _, last}), ..._] =>
    let status = insts |> TestMap.joint_status |> TestStatus.to_string;
    let pos = DecUtil.abs_position(~font_metrics, last);
    Some(
      Node.div(
        ~attr=Attr.many([Attr.classes(["test-result", status]), pos]),
        [],
      ),
    );
  | _ => None
  };

let test_result_layer =
    (~font_metrics, ~measured: Measured.t, test_results: TestResults.t)
    : list(Node.t) =>
  List.filter_map(
    ((id, insts)) =>
      switch (Id.Map.find_opt(id, measured.tiles)) {
      | Some(ms) => test_status_icon_view(~font_metrics, insts, ms)
      | None => None
      },
    test_results.test_map,
  );

let deco =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~selected,
      ~error_ids,
      ~test_results: option(TestResults.t),
      ~highlights: option(ColorSteps.colorMap),
      ~next_steps: list(Id.t),
      ~inject,
      {
        state: {
          zipper,
          meta: {term_ranges, segment, measured, terms, tiles, _},
          _,
        },
        _,
      }: Editor.t,
    ) => {
  module Deco =
    Deco.Deco({
      let map = measured;
      let terms = terms;
      let term_ranges = term_ranges;
      let tiles = tiles;
      let font_metrics = font_metrics;
      let show_backpack_targets = show_backpack_targets;
      let error_ids = error_ids;
      let next_steps = next_steps;
    });
  let inject = i => inject(StepSelected(i));
  let decos =
    (selected ? Deco.all(zipper, segment) : Deco.err_holes(zipper))
    @ Deco.next_steps(zipper, ~inject);
  let decos =
    switch (test_results) {
    | None => decos
    | Some(test_results) =>
      decos @ test_result_layer(~font_metrics, ~measured, test_results) // TODO move into decos
    };
  switch (highlights) {
  | Some(colorMap) =>
    decos @ Deco.color_highlights(ColorSteps.to_list(colorMap))
  | _ => decos
  };
};

let view =
    (
      ~select as _: unit => Ui_effect.t(unit),
      ~inject: event => Ui_effect.t(unit),
      ~ui_state as
        {font_metrics, show_backpack_targets, mousedown, _}: Model.ui_state,
      ~settings: Settings.t,
      ~selected: bool=true,
      ~locked=false,
      ~test_results: option(TestResults.t),
      ~highlights: option(ColorSteps.colorMap),
      ~overlayer: option(Node.t)=None,
      ~error_ids: list(Id.t),
      ~sort=Sort.root,
      ~next_steps,
      editor: Editor.t,
    ) => {
  let code_text_view = Code.view(~sort, ~font_metrics, ~settings, editor);
  let deco_view =
    deco(
      ~font_metrics,
      ~show_backpack_targets,
      ~selected,
      ~error_ids,
      ~test_results,
      ~highlights,
      ~next_steps,
      ~inject,
      editor,
    );
  let code_view =
    Node.div(
      ~attr=Attr.many([Attr.classes(["code-container"])]),
      [code_text_view] @ deco_view @ Option.to_list(overlayer),
    );
  let mousedown_overlay =
    selected && mousedown ? [mousedown_overlay(~inject, ~font_metrics)] : [];
  let on_mousedown =
    locked
      ? _ =>
          Virtual_dom.Vdom.Effect.(Many([Prevent_default, Stop_propagation]))
      : mousedown_handler(~inject, ~font_metrics);

  Node.div(
    ~attr=
      Attr.many([
        Attr.classes(["cell-item"]),
        Attr.on_mousedown(on_mousedown),
      ]),
    mousedown_overlay @ [code_view],
  );
};
