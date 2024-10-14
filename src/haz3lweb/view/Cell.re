open Util;
open Virtual_dom.Vdom;
open Haz3lcore;
open Node;

let get_goal = (~font_metrics: FontMetrics.t, ~target_id, e) => {
  let rect = JsUtil.get_elem_by_id(target_id)##getBoundingClientRect;
  let goal_x = float_of_int(e##.clientX);
  let goal_y = float_of_int(e##.clientY);
  Point.{
    row: Float.to_int((goal_y -. rect##.top) /. font_metrics.row_height),
    col:
      Float.(
        to_int(round((goal_x -. rect##.left) /. font_metrics.col_width))
      ),
  };
};

let mousedown_overlay = (~inject, ~font_metrics, ~target_id) =>
  div(
    ~attrs=
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
    /* Note that we only trigger drag mode (set mousedown)
     * when the left mouse button (aka button 0) is pressed */
    Virtual_dom.Vdom.Effect.Many(
      List.map(
        inject,
        Update.(
          (JsUtil.mouse_button(evt) == 0 ? [SetMeta(Mousedown)] : [])
          @ mousedown_updates
          @ [PerformAction(Move(Goal(Point(goal))))]
        ),
      ),
    );
  | (false, n) => inject(PerformAction(Select(Smart(n))))
  };

let narrative_cell = (content: Node.t) =>
  div(
    ~attrs=[Attr.class_("cell")],
    [div(~attrs=[Attr.class_("cell-chapter")], [content])],
  );

let simple_cell_item = (content: list(Node.t)) =>
  div(~attrs=[Attr.classes(["cell-item"])], content);

let caption = (~rest: option(string)=?, bolded: string) =>
  div(
    ~attrs=[Attr.classes(["cell-caption"])],
    [strong([text(bolded)])] @ (rest |> Option.map(text) |> Option.to_list),
  );

let simple_cell_view = (items: list(t)) =>
  div(~attrs=[Attr.class_("cell")], items);

let test_status_icon_view =
    (~font_metrics, insts, ms: Measured.Shards.t): option(t) =>
  switch (ms) {
  | [(_, {origin: _, last}), ..._] =>
    let status = insts |> TestMap.joint_status |> TestStatus.to_string;
    let pos = DecUtil.abs_position(~font_metrics, last);
    Some(div(~attrs=[Attr.classes(["test-result", status]), pos], []));
  | _ => None
  };

let test_result_layer =
    (~font_metrics, ~measured: Measured.t, test_results: TestResults.t): t =>
  Web.div_c(
    "test-decos",
    List.filter_map(
      ((id, insts)) =>
        switch (Id.Map.find_opt(id, measured.tiles)) {
        | Some(ms) => test_status_icon_view(~font_metrics, insts, ms)
        | None => None
        },
      test_results.test_map,
    ),
  );

let deco =
    (
      ~inject,
      ~ui_state,
      ~selected,
      ~test_results: option(TestResults.t),
      ~highlights: option(ColorSteps.colorMap),
      z,
      meta: Editor.Meta.t,
    ) => {
  module Deco =
    Deco.Deco({
      let ui_state = ui_state;
      let meta = meta;
      let highlights = highlights;
    });
  let decos = selected ? Deco.all(z) : Deco.always();
  let decos =
    decos
    @ [
      ProjectorView.all(
        z,
        ~meta,
        ~inject,
        ~font_metrics=ui_state.font_metrics,
      ),
    ];
  switch (test_results) {
  | None => decos
  | Some(test_results) =>
    decos
    @ [
      test_result_layer(
        ~font_metrics=ui_state.font_metrics,
        ~measured=meta.syntax.measured,
        test_results,
      ),
    ] // TODO move into decos
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
    | _ => result.elab.d
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
      ~infomap=Id.Map.empty,
      dhexp,
    );
  let exn_view =
    switch (result.evaluation) {
    | ResultFail(err) => [
        div(
          ~attrs=[Attr.classes(["error-msg"])],
          [text(error_msg(err))],
        ),
      ]
    | _ => []
    };
  div(
    ~attrs=[Attr.classes(["cell-item", "cell-result"])],
    exn_view
    @ [
      div(
        ~attrs=[Attr.classes(["status", status_of(result.evaluation)])],
        [
          div(~attrs=[Attr.classes(["spinner"])], []),
          div(~attrs=[Attr.classes(["eq"])], [text("≡")]),
        ],
      ),
      div(
        ~attrs=[Attr.classes(["result", status_of(result.evaluation)])],
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
      ~read_only=false,
      s,
    )
  };

let editor_view =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~ui_state: Model.ui_state,
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
      ~sort=Sort.root,
      ~override_statics: option(Editor.CachedStatics.t)=?,
      editor: Editor.t,
    ) => {
  let Model.{font_metrics, mousedown, _} = ui_state;
  let meta =
    /* For exercises modes */
    switch (override_statics) {
    | None => editor.state.meta
    | Some(statics) => {...editor.state.meta, statics}
    };
  let mousedown_overlay =
    selected && mousedown
      ? [mousedown_overlay(~inject, ~font_metrics, ~target_id)] : [];
  let code_text_view =
    Code.view(~sort, ~font_metrics, ~settings, editor.state.zipper, meta);
  let deco_view =
    deco(
      ~inject,
      ~ui_state,
      ~selected,
      ~test_results,
      ~highlights,
      editor.state.zipper,
      meta,
    );

  let code_view =
    div(
      ~attrs=[Attr.id(target_id), Attr.classes(["code-container"])],
      [code_text_view]
      @ deco_view
      @ Option.to_list(overlayer)
      @ mousedown_overlay,
    );
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
    ~attrs=[
      Attr.classes([
        "cell",
        selected ? "selected" : "deselected",
        locked ? "locked" : "unlocked",
      ]),
    ],
    [
      div(
        ~attrs=[
          Attr.classes(["cell-item"]),
          Attr.on_mousedown(on_mousedown),
        ],
        Option.to_list(caption) @ [code_view],
      ),
    ]
    @ (footer |> Option.to_list |> List.concat),
  );
};

let report_footer_view = content => {
  div(~attrs=[Attr.classes(["cell-item", "cell-report"])], content);
};

let test_report_footer_view = (~inject, ~test_results: option(TestResults.t)) => {
  report_footer_view([TestView.test_summary(~inject, ~test_results)]);
};

let panel = (~classes=[], content, ~footer: option(t)) => {
  simple_cell_view(
    [
      div(~attrs=[Attr.classes(["cell-item", "panel"] @ classes)], content),
    ]
    @ Option.to_list(footer),
  );
};

let title_cell = title => {
  simple_cell_view([
    div(
      ~attrs=[Attr.class_("title-cell")],
      [div(~attrs=[Attr.class_("title-text")], [text(title)])],
    ),
  ]);
};

let wrong_impl_caption = (~inject, sub: string, n: int) => {
  div(
    ~attrs=[Attr.class_("wrong-impl-cell-caption")],
    [
      caption("", ~rest=sub),
      div(
        ~attrs=[Attr.class_("instructor-edit-icon")],
        [
          Widgets.button(
            Icons.delete,
            _ => inject(UpdateAction.DeleteBuggyImplementation(n)),
            ~tooltip="Delete Buggy Implementation",
          ),
        ],
      ),
    ],
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
    ~overlayer=Some(expander_deco),
    ~sort,
    segment
    |> Zipper.unzip
    |> Editor.init(~settings=CoreSettings.off, ~read_only=true),
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
  let editor =
    segment
    |> Zipper.unzip
    |> Editor.init(~settings=settings.core, ~read_only=true);
  let statics = editor.state.meta.statics;
  let elab =
    settings.core.elaborate || settings.core.dynamics
      ? Interface.elaborate(
          ~settings=settings.core,
          statics.info_map,
          statics.term,
        )
      : DHExp.Bool(true) |> DHExp.fresh;
  let elab: Elaborator.Elaboration.t = {d: elab};
  let result: ModelResult.t =
    settings.core.dynamics
      ? Evaluation({
          elab,
          evaluation: Interface.evaluate(~settings=settings.core, elab.d),
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
    editor,
  );
};
