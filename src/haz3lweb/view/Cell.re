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
    ~attr=Attr.class_("cell"),
    [div(~attr=Attr.class_("cell-chapter"), [content])],
  );

let simple_cell_item = (content: list(Node.t)) =>
  div(~attr=Attr.classes(["cell-item"]), content);

let caption = (~rest: option(string)=?, bolded: string) =>
  div(
    ~attr=Attr.many([Attr.classes(["cell-caption"])]),
    [strong([text(bolded)])] @ (rest |> Option.map(text) |> Option.to_list),
  );

let simple_cell_view = (items: list(t)) =>
  div(~attr=Attr.class_("cell"), items);

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
    (~font_metrics, ~measured: Measured.t, test_results: TestResults.t)
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
      ~font_metrics,
      ~show_backpack_targets,
      ~selected,
      ~error_ids,
      ~test_results: option(TestResults.t),
      ~highlights: option(ColorSteps.colorMap),
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
    });
  let decos = selected ? Deco.all(zipper, segment) : Deco.err_holes(zipper);
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

let error_msg = (err: ProgramResult.error) =>
  switch (err) {
  | EvaulatorError(err) => EvaluatorError.show(err)
  | UnknownException(str) => str
  | Timeout => "Evaluation timed out"
  };

let status_of: ProgramResult.t => string =
  fun
  | ResultPending => "pending"
  | ResultOk(_) => "ok"
  | ResultFail(_) => "fail"
  | Off(_) => "off";

let live_eval =
    (
      ~inject,
      ~ui_state as {font_metrics, _}: Model.ui_state,
      ~result_key: string,
      ~settings: Settings.t,
      ~locked,
      result: ModelResult.eval_result,
    ) => {
  open Node;
  let dhexp =
    switch (result.evaluation, result.previous) {
    | (ResultOk(res), _) => ProgramResult.get_dhexp(res)
    | (ResultPending, ResultOk(res)) => ProgramResult.get_dhexp(res)
    | _ => result.elab
    };
  let dhcode_view =
    DHCode.view(
      ~locked,
      ~inject,
      ~settings=settings.core.evaluation,
      ~selected_hole_instance=None,
      ~font_metrics,
      ~width=80,
      ~result_key,
      dhexp,
    );
  let exn_view =
    switch (result.evaluation) {
    | ResultFail(err) => [
        div(~attr=Attr.classes(["error-msg"]), [text(error_msg(err))]),
      ]
    | _ => []
    };
  div(
    ~attr=Attr.classes(["cell-item", "cell-result"]),
    exn_view
    @ [
      div(
        ~attr=Attr.classes(["status", status_of(result.evaluation)]),
        [
          div(~attr=Attr.classes(["spinner"]), []),
          div(~attr=Attr.classes(["eq"]), [text("≡")]),
        ],
      ),
      div(
        ~attr=Attr.classes(["result", status_of(result.evaluation)]),
        [dhcode_view],
      ),
      Widgets.toggle(~tooltip="Show Stepper", "s", false, _ =>
        inject(UpdateAction.ToggleStepper(result_key))
      ),
    ],
  );
};

let footer =
    (
      ~locked,
      ~inject,
      ~ui_state as {font_metrics, _} as ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~result: ModelResult.t,
      ~result_key,
    ) =>
  switch (result) {
  | _ when !settings.core.dynamics => []
  | NoElab => []
  | Evaluation(result) => [
      live_eval(~locked, ~inject, ~ui_state, ~settings, ~result_key, result),
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

let editor_view =
    (
      ~inject,
      ~ui_state as
        {font_metrics, show_backpack_targets, mousedown, _}: Model.ui_state,
      ~settings: Settings.t,
      ~target_id: string,
      ~mousedown_updates: list(Update.t)=[],
      ~selected: bool=true,
      ~locked=false,
      ~caption: option(Node.t)=?,
      ~test_results: option(TestResults.t),
      ~footer: option(list(Node.t))=?,
      ~highlights: option(ColorSteps.colorMap),
      ~overlayer: option(Node.t)=None,
      ~error_ids: list(Id.t),
      ~sort=Sort.root,
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
      editor,
    );
  let code_view =
    div(
      ~attr=
        Attr.many([Attr.id(target_id), Attr.classes(["code-container"])]),
      [code_text_view] @ deco_view @ Option.to_list(overlayer),
    );
  let mousedown_overlay =
    selected && mousedown
      ? [mousedown_overlay(~inject, ~font_metrics, ~target_id)] : [];
  let on_mousedown =
    locked
      ? _ =>
          Virtual_dom.Vdom.Effect.(Many([Prevent_default, Stop_propagation]))
      : mousedown_handler(
          ~inject,
          ~font_metrics,
          ~target_id,
          ~mousedown_updates,
        );
  div(
    ~attr=
      Attr.classes([
        "cell",
        selected ? "selected" : "deselected",
        locked ? "locked" : "unlocked",
      ]),
    [
      div(
        ~attr=
          Attr.many([
            Attr.classes(["cell-item"]),
            Attr.on_mousedown(on_mousedown),
          ]),
        Option.to_list(caption) @ mousedown_overlay @ [code_view],
      ),
    ]
    @ (footer |> Option.to_list |> List.concat),
  );
};

let report_footer_view = content => {
  div(~attr=Attr.classes(["cell-item", "cell-report"]), content);
};

let test_report_footer_view = (~inject, ~test_results: option(TestResults.t)) => {
  report_footer_view([TestView.test_summary(~inject, ~test_results)]);
};

let panel = (~classes=[], content, ~footer: option(t)) => {
  simple_cell_view(
    [div(~attr=Attr.classes(["cell-item", "panel"] @ classes), content)]
    @ Option.to_list(footer),
  );
};

/* An editor view that is not selectable or editable,
 * and does not show error holes or test results.
 * Used in Docs to display the header example */
let locked_no_statics =
    (
      ~inject,
      ~ui_state,
      ~segment,
      ~highlights,
      ~settings,
      ~sort,
      ~expander_deco,
      ~target_id,
    ) => [
  editor_view(
    ~locked=true,
    ~selected=false,
    ~highlights,
    ~inject,
    ~ui_state,
    ~settings,
    ~target_id,
    ~footer=[],
    ~test_results=None,
    ~error_ids=[],
    ~overlayer=Some(expander_deco),
    ~sort,
    segment |> Zipper.unzip |> Editor.init(~read_only=true),
  ),
];

/* An editor view that is not selectable or editable,
 * but does show static errors, test results, and live values.
 * Used in Docs for examples */
let locked =
    (
      ~ui_state,
      ~settings: Settings.t,
      ~inject,
      ~target_id,
      ~segment: Segment.t,
    ) => {
  let editor = segment |> Zipper.unzip |> Editor.init(~read_only=true);
  let statics =
    settings.core.statics
      ? ScratchSlide.mk_statics(~settings, editor, Builtins.ctx_init)
      : CachedStatics.empty_statics;
  let elab =
    settings.core.elaborate || settings.core.dynamics
      ? Interface.elaborate(
          ~settings=settings.core,
          statics.info_map,
          editor.state.meta.view_term,
        )
      : DHExp.BoolLit(true);
  let result: ModelResult.t =
    settings.core.dynamics
      ? Evaluation({
          elab,
          evaluation: Interface.evaluate(~settings=settings.core, elab),
          previous: ResultPending,
        })
      : NoElab;
  let footer =
    settings.core.elaborate || settings.core.dynamics
      ? footer(
          ~locked=true,
          ~inject,
          ~settings,
          ~ui_state,
          ~result_key=target_id,
          ~result,
        )
      : [];
  editor_view(
    ~locked=true,
    ~selected=false,
    ~highlights=None,
    ~inject,
    ~ui_state,
    ~settings,
    ~target_id,
    ~footer,
    ~test_results=ModelResult.test_results(result),
    ~error_ids=statics.error_ids,
    editor,
  );
};
