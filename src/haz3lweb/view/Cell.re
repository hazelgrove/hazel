open Virtual_dom.Vdom;
open Haz3lcore;

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
  Node.div(
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
  Node.div(
    ~attr=Attr.class_("cell-container"),
    [Node.div(~attr=Attr.class_("cell-chapter"), [content])],
  );

let simple_cell_item = (content: list(Node.t)) =>
  Node.div(~attr=Attr.classes(["cell", "cell-item"]), content);

let cell_caption = (content: list(Node.t)) =>
  Node.div(~attr=Attr.many([Attr.classes(["cell-caption"])]), content);

let simple_caption = (caption: string) =>
  cell_caption([Node.text(caption)]);

let bolded_caption = (~rest: option(string)=?, bolded: string) =>
  cell_caption(
    [Node.strong([Node.text(bolded)])]
    @ (rest |> Option.map(rest => Node.text(rest)) |> Option.to_list),
  );

let simple_cell_view = (items: list(Node.t)) =>
  Node.div(~attr=Attr.class_("cell-container"), items);

let code_cell_view =
    (
      ~inject,
      ~font_metrics,
      ~clss=[],
      ~selected: bool,
      ~mousedown: bool,
      ~mousedown_updates: list(Update.t)=[],
      ~code_id: string,
      ~caption: option(Node.t)=?,
      code: Node.t,
      footer: list(Node.t),
    )
    : Node.t => {
  // TODO: why is this in here? doesn't it cover the whole screen?
  let mousedown_overlay =
    selected && mousedown
      ? [mousedown_overlay(~inject, ~font_metrics, ~target_id=code_id)] : [];
  let code = mousedown_overlay @ [code];
  Node.div(
    ~attr=Attr.class_("cell-container"),
    [
      Node.div(
        ~attr=
          Attr.many([
            Attr.classes(
              ["cell-item", "cell", ...clss]
              @ (selected ? ["selected"] : ["deselected"]),
            ),
            Attr.on_mousedown(
              mousedown_handler(
                ~inject,
                ~font_metrics,
                ~target_id=code_id,
                ~mousedown_updates,
              ),
            ),
          ]),
        Option.to_list(caption) @ code,
      ),
    ]
    @ footer,
  );
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
    (
      ~font_metrics,
      ~measured: Measured.t,
      test_results: TestResults.test_results,
    )
    : list(Node.t) => {
  //print_endline(Interface.show_test_results(test_results));
  List.filter_map(
    ((id, insts)) =>
      switch (Id.Map.find_opt(id, measured.tiles)) {
      | Some(ms) => test_status_icon_view(~font_metrics, insts, ms)
      | _ => None
      },
    test_results.test_map,
  );
};

let deco =
    (
      ~zipper,
      ~measured,
      ~term_ranges,
      ~unselected,
      ~segment,
      ~font_metrics,
      ~show_backpack_targets,
      ~selected,
      ~info_map,
      ~test_results: option(TestResults.test_results),
      ~color_highlighting: option(ColorSteps.colorMap),
    ) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = measured;
      let show_backpack_targets = show_backpack_targets;
      let (_term, terms) = MakeTerm.go(unselected);
      let info_map = info_map;
      let term_ranges = term_ranges;
      let tiles = TileMap.mk(unselected);
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

let eval_result_footer_view =
    (
      ~inject,
      ~font_metrics,
      ~elab,
      ~settings: Settings.t,
      ~result_key,
      simple: TestResults.simple,
    ) => {
  let show_stepper =
    Widgets.toggle(~tooltip="Show Stepper", "s", false, _ =>
      inject(UpdateAction.ToggleStepper(result_key))
    );
  let d_view =
    switch (simple) {
    | None => [
        Node.text("No result available. Elaboration follows:"),
        DHCode.view(
          ~inject,
          ~settings=settings.core.evaluation,
          ~selected_hole_instance=None,
          ~font_metrics,
          ~width=80,
          ~result_key,
          elab,
        ),
      ]
    | Some({eval_result, _}) => [
        DHCode.view(
          ~inject,
          ~settings=settings.core.evaluation,
          ~selected_hole_instance=None,
          ~font_metrics,
          ~width=80,
          ~result_key,
          eval_result,
        ),
      ]
    };
  Node.[
    div(
      ~attr=Attr.classes(["cell-item", "cell-result"]),
      [
        div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
        div(~attr=Attr.classes(["result"]), d_view),
        show_stepper,
      ],
    ),
  ];
};

let editor_view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~clss=[],
      ~mousedown: bool,
      ~mousedown_updates: list(Update.t)=[],
      ~settings: Settings.t,
      ~selected: bool,
      ~caption: option(Node.t)=?,
      ~code_id: string,
      ~info_map: Statics.Map.t,
      ~test_results: option(TestResults.test_results),
      ~footer: list(Node.t),
      ~color_highlighting: option(ColorSteps.colorMap),
      editor: Editor.t,
    ) => {
  let zipper = editor.state.zipper;
  let term_ranges = editor.state.meta.term_ranges;
  let segment = Zipper.zip(zipper);
  let unselected = Zipper.unselect_and_zip(zipper);
  let measured = editor.state.meta.measured;
  let buffer_ids: list(Uuidm.t) = {
    /* Collect ids of tokens in buffer for styling purposes. This is
     * currently necessary as the selection is not persisted through
     * unzipping for display */
    let buffer =
      Selection.is_buffer(zipper.selection) ? zipper.selection.content : [];
    Id.Map.bindings(Measured.of_segment(buffer).tiles) |> List.map(fst);
  };
  let code_base_view =
    Code.view(
      ~sort=Sort.root,
      ~font_metrics,
      ~buffer_ids,
      ~segment,
      ~unselected,
      ~measured,
      ~settings,
    );
  let deco_view =
    deco(
      ~zipper,
      ~unselected,
      ~measured,
      ~term_ranges,
      ~segment,
      ~font_metrics,
      ~show_backpack_targets,
      ~selected,
      ~info_map,
      ~test_results,
      ~color_highlighting,
    );
  let code_view =
    Node.div(
      ~attr=Attr.many([Attr.id(code_id), Attr.classes(["code-container"])]),
      [code_base_view] @ deco_view,
    );
  code_cell_view(
    ~inject,
    ~font_metrics,
    ~clss,
    ~selected,
    ~mousedown,
    ~mousedown_updates,
    ~code_id,
    ~caption?,
    code_view,
    footer,
  );
};

let editor_with_result_view =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~clss=[],
      ~mousedown: bool,
      ~settings: Settings.t,
      ~color_highlighting: option(ColorSteps.colorMap),
      ~selected: bool,
      ~caption: option(Node.t)=?,
      ~code_id: string,
      ~info_map: Statics.Map.t,
      ~result_key: string,
      ~result: ModelResult.t,
      editor: Editor.t,
    ) => {
  let simple = ModelResult.get_simple(result);
  let test_results = TestResults.unwrap_test_results(simple);
  let eval_result_footer =
    if (!settings.core.statics) {
      [];
    } else {
      switch (result) {
      | NoElab => []
      | Evaluation({elab, _}) =>
        eval_result_footer_view(
          ~inject,
          ~font_metrics,
          ~elab,
          ~settings,
          ~result_key,
          simple,
        )
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
  editor_view(
    ~inject,
    ~font_metrics,
    ~show_backpack_targets,
    ~clss,
    ~mousedown,
    ~mousedown_updates=[],
    ~settings,
    ~selected,
    ~caption?,
    ~code_id,
    ~info_map,
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
  Node.(
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
    )
  );

let report_footer_view = content => {
  Node.(div(~attr=Attr.classes(["cell-item", "cell-report"]), content));
};

let test_report_footer_view =
    (~inject, ~test_results: option(TestResults.test_results)) => {
  report_footer_view([TestView.test_summary(~inject, ~test_results)]);
};

let panel = (~classes=[], content, ~footer: option(Node.t)) => {
  simple_cell_view(
    [
      Node.div(
        ~attr=Attr.classes(["cell-item", "panel"] @ classes),
        content,
      ),
    ]
    @ Option.to_list(footer),
  );
};

let title_cell = title => {
  simple_cell_view([
    Node.(
      div(
        ~attr=Attr.class_("title-cell"),
        [Node.(div(~attr=Attr.class_("title-text"), [text(title)]))],
      )
    ),
  ]);
};
