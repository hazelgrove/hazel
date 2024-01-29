open Virtual_dom.Vdom;
open Haz3lcore;
open Node;

let get_goal = (~font_metrics: FontMetrics.t, ~target_id, e) => {
  let rect = JsUtil.get_elem_by_id(target_id)##getBoundingClientRect;
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

let mousedown_overlay = (~inject, ~font_metrics, ~target_id) =>
  div(
    ~attr=
      Attr.many(
        Attr.[
          id("mousedown-overlay"),
          on_mouseup(_ => inject(Update.SetMeta(Mouseup))),
          on_mousemove(e => {
            let goal = get_goal(~font_metrics, ~target_id, e);
            inject(
              Update.PerformAction(Select(Resize(Goal(Point(goal))))),
            );
          }),
        ],
      ),
    [],
  );

let mousedown_handler =
    (
      ~inject: UpdateAction.t => 'a,
      ~font_metrics,
      ~target_id,
      ~mousedown_updates,
      evt,
    ) =>
  switch (JsUtil.ctrl_held(evt), JsUtil.num_clicks(evt)) {
  | (true, _) =>
    let goal = get_goal(~font_metrics, ~target_id, evt);

    let events = [
      inject(PerformAction(Move(Goal(Point(goal))))),
      inject(PerformAction(Jump(BindingSiteOfIndicatedVar))),
    ];
    Virtual_dom.Vdom.Effect.Many(events);
  | (false, 1) =>
    let goal = get_goal(~font_metrics, ~target_id, evt);
    Virtual_dom.Vdom.Effect.Many(
      List.map(
        inject,
        Update.(
          [SetMeta(Mousedown)]
          @ mousedown_updates
          @ [PerformAction(Move(Goal(Point(goal))))]
        ),
      ),
    );
  | (false, 2) => inject(PerformAction(Select(Tile(Current))))
  | (false, 3 | _) => inject(PerformAction(Select(Smart)))
  };

let narrative_cell = (content: Node.t) =>
  div(
    ~attr=Attr.class_("cell-container"),
    [div(~attr=Attr.class_("cell-chapter"), [content])],
  );

let simple_cell_item = (content: list(Node.t)) =>
  div(~attr=Attr.classes(["cell", "cell-item"]), content);

let caption = (~rest: option(string)=?, bolded: string) =>
  div(
    ~attr=Attr.many([Attr.classes(["cell-caption"])]),
    [strong([text(bolded)])] @ (rest |> Option.map(text) |> Option.to_list),
  );

let simple_cell_view = (items: list(t)) =>
  div(~attr=Attr.class_("cell-container"), items);

let test_status_icon_view =
    (~font_metrics, insts, ms: Measured.Shards.t): option(t) =>
  switch (ms) {
  | [(_, {origin: _, last}), ..._] =>
    let status = insts |> TestMap.joint_status |> TestStatus.to_string;
    let pos = DecUtil.abs_position(~font_metrics, last);
    Some(
      div(
        ~attr=Attr.many([Attr.classes(["test-result", status]), pos]),
        [],
      ),
    );
  | _ => None
  };

let test_result_layer =
    (
      ~font_metrics,
      ~measured: Measured.t,
      test_results: TestResults.test_results,
    )
    : list(t) =>
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
      ~syntax as
        {zipper, term_ranges, segment, measured, terms, tiles, _}: Editor.syntax,
      ~font_metrics,
      ~show_backpack_targets,
      ~selected,
      ~error_ids,
      ~test_results: option(TestResults.test_results),
      ~color_highlighting: option(ColorSteps.colorMap),
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
    });
  let decos = selected ? Deco.all(zipper, segment) : Deco.err_holes(zipper);
  let decos =
    switch (test_results) {
    | None => decos
    | Some(test_results) =>
      decos @ test_result_layer(~font_metrics, ~measured, test_results) // TODO move into decos
    };
  switch (color_highlighting, selected) {
  | (Some(colorMap), true) =>
    decos @ Deco.color_highlights(ColorSteps.to_list(colorMap))
  | _ => decos
  };
};

let footer =
    (
      ~inject,
      ~settings: Settings.t,
      ~ui_state as {font_metrics, _}: Model.ui_state,
      ~result_key: string,
      result: ModelResult.elab_eval,
    ) => {
  let show_stepper =
    Widgets.toggle(~tooltip="Show Stepper", "s", false, _ =>
      inject(UpdateAction.ToggleStepper(result_key))
    );
  let dhcode_view = (~show_casts) =>
    DHCode.view(
      ~settings={...CoreSettings.Evaluation.init, show_casts},
      ~selected_hole_instance=None,
      ~font_metrics,
      ~width=80,
    );
  let d_view: list(t) =
    switch (result.evaluation, result.elab) {
    | (ResultOk(res), _) when settings.core.dynamics =>
      let dhexp = ProgramResult.get_dhexp(res);
      /* Disabling casts in this case as large casts can blow up UI perf */
      [dhcode_view(~inject, ~result_key, ~show_casts=false, dhexp)];
    | (_, elab) when settings.core.elaborate && !settings.core.dynamics => [
        text("Evaluation disabled, elaboration follows:"),
        dhcode_view(~inject, ~result_key, ~show_casts=true, elab),
      ]
    | _ => [text("Evaluation & elaboration display disabled")]
    };
  div(
    ~attr=Attr.classes(["cell-item", "cell-result"]),
    [
      div(~attr=Attr.class_("equiv"), [text("≡")]),
      div(~attr=Attr.classes(["result"]), d_view),
      show_stepper,
    ],
  );
};

let editor_view =
    (
      ~inject,
      ~ui_state as
        {font_metrics, show_backpack_targets, mousedown, _}: Model.ui_state,
      ~settings: Settings.t,
      ~code_id: string,
      ~clss=[],
      ~mousedown_updates: list(Update.t)=[],
      ~selected: bool=true,
      ~caption: option(Node.t)=?,
      ~test_results: option(TestResults.test_results),
      ~footer: list(Node.t),
      ~color_highlighting: option(ColorSteps.colorMap),
      ~error_ids: list(Id.t),
      syntax: Editor.syntax,
    ) => {
  let code_text_view =
    Code.view(~sort=Sort.root, ~font_metrics, ~syntax, ~settings);
  let deco_view =
    deco(
      ~syntax,
      ~font_metrics,
      ~show_backpack_targets,
      ~selected,
      ~error_ids,
      ~test_results,
      ~color_highlighting,
    );
  let code_view =
    div(
      ~attr=Attr.many([Attr.id(code_id), Attr.classes(["code-container"])]),
      [code_text_view] @ deco_view,
    );
  let mousedown_overlay =
    selected && mousedown
      ? [mousedown_overlay(~inject, ~font_metrics, ~target_id=code_id)] : [];
  let classes = ["cell-item", "cell", selected ? "selected" : "deselected"];
  div(
    ~attr=Attr.class_("cell-container"),
    [
      div(
        ~attr=
          Attr.many([
            Attr.classes(classes @ clss),
            Attr.on_mousedown(
              mousedown_handler(
                ~inject,
                ~font_metrics,
                ~target_id=code_id,
                ~mousedown_updates,
              ),
            ),
          ]),
        Option.to_list(caption) @ mousedown_overlay @ [code_view],
      ),
    ]
    @ footer,
  );
};

let footer =
    (
      ~inject,
      ~ui_state as {font_metrics, _} as ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~result: ModelResult.t,
      ~result_key,
    ) =>
  if (!settings.core.statics) {
    [];
  } else {
    switch (result) {
    | NoElab => []
    | Evaluation(result) => [
        footer(~inject, ~ui_state, ~settings, ~result_key, result),
      ]
    | Stepper(s) =>
      StepperView.stepper_view(
        ~inject,
        ~settings=settings.core.evaluation,
        ~font_metrics,
        ~result_key,
        s,
      )
    };
  };

let editor_with_result_view =
    (
      ~inject,
      ~ui_state,
      ~clss=[],
      ~settings: Settings.t,
      ~color_highlighting: option(ColorSteps.colorMap),
      ~selected: bool,
      ~caption: option(Node.t)=?,
      ~code_id: string,
      ~result_key: string,
      ~result: ModelResult.t,
      editor,
    ) => {
  let simple = ModelResult.get_simple(result);
  let test_results = TestResults.unwrap_test_results(simple);
  let eval_result_footer =
    footer(~inject, ~ui_state, ~settings, ~result_key, ~result);
  editor_view(
    ~inject,
    ~ui_state,
    ~clss,
    ~mousedown_updates=[],
    ~settings,
    ~selected,
    ~caption?,
    ~code_id,
    ~test_results,
    ~footer=eval_result_footer,
    ~color_highlighting,
    editor,
  );
};

let test_view =
    (
      ~settings,
      ~title,
      ~inject,
      ~font_metrics,
      ~test_results: option(TestResults.test_results),
    )
    : Node.t =>
  div(
    ~attr=Attr.classes(["cell-item", "panel", "test-panel"]),
    [
      TestView.view_of_main_title_bar(title),
      TestView.test_reports_view(
        ~settings,
        ~inject,
        ~font_metrics,
        ~test_results,
      ),
      TestView.test_summary(~inject, ~test_results),
    ],
  );

let report_footer_view = content => {
  div(~attr=Attr.classes(["cell-item", "cell-report"]), content);
};

let test_report_footer_view =
    (~inject, ~test_results: option(TestResults.test_results)) => {
  report_footer_view([TestView.test_summary(~inject, ~test_results)]);
};

let panel = (~classes=[], content, ~footer: option(t)) => {
  simple_cell_view(
    [div(~attr=Attr.classes(["cell-item", "panel"] @ classes), content)]
    @ Option.to_list(footer),
  );
};

let title_cell = title => {
  simple_cell_view([
    div(
      ~attr=Attr.class_("title-cell"),
      [div(~attr=Attr.class_("title-text"), [text(title)])],
    ),
  ]);
};
