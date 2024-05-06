open Haz3lcore;
open Virtual_dom.Vdom;
open Sexplib.Std;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type editor_selection =
  | MainEditor
  | Stepper(int);

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
      ~settings,
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

let stepper_view =
    (
      ~inject,
      ~settings,
      ~font_metrics,
      ~result_key,
      ~read_only: bool,
      stepper: Stepper.t,
    ) => {
  let step_dh_code =
      (
        ~next_steps,
        {previous_step, hidden_steps, chosen_step, d}: Stepper.step_info,
      ) =>
    div(
      ~attr=Attr.classes(["result"]),
      [
        DHCode.view(
          ~inject,
          ~settings,
          ~selected_hole_instance=None,
          ~font_metrics,
          ~width=80,
          ~previous_step,
          ~chosen_step,
          ~hidden_steps,
          ~result_key,
          ~next_steps,
          ~infomap=Id.Map.empty,
          d,
        ),
      ],
    );
  let history =
    Stepper.get_history(~settings=settings.core.evaluation, stepper);
  switch (history) {
  | [] => []
  | [hd, ...tl] =>
    let button_back =
      Widgets.button_d(
        Icons.undo,
        inject(UpdateAction.StepperAction(result_key, StepBackward)),
        ~disabled=
          !Stepper.can_undo(~settings=settings.core.evaluation, stepper),
        ~tooltip="Step Backwards",
      );
    let button_hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
        inject(UpdateAction.ToggleStepper(result_key))
      );
    let toggle_show_history =
      Widgets.toggle(
        ~tooltip="Show History",
        "h",
        settings.core.evaluation.stepper_history,
        _ =>
        inject(Set(Evaluation(ShowRecord)))
      );
    let eval_settings =
      Widgets.button(Icons.gear, _ =>
        inject(Set(Evaluation(ShowSettings)))
      );
    let current =
      div(
        ~attr=Attr.classes(["cell-item", "cell-result"]),
        read_only
          ? [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            step_dh_code(~next_steps=[], hd),
          ]
          : [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            step_dh_code(
              ~next_steps=
                List.mapi(
                  (i, x: EvaluatorStep.EvalObj.t) =>
                    (i, x.d_loc |> DHExp.rep_id),
                  Stepper.get_next_steps(stepper),
                ),
              hd,
            ),
            button_back,
            eval_settings,
            toggle_show_history,
            button_hide_stepper,
          ],
      );
    let dh_code_previous = step_dh_code;
    let rec previous_step =
            (~hidden: bool, step: Stepper.step_info): list(Node.t) => {
      let hidden_steps =
        settings.core.evaluation.show_hidden_steps
          ? Stepper.hidden_steps_of_info(step)
            |> List.rev_map(previous_step(~hidden=true))
            |> List.flatten
          : [];
      [
        div(
          ~attr=
            Attr.classes(
              ["cell-item", "cell-result"] @ (hidden ? ["hidden"] : []),
            ),
          [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            dh_code_previous(~next_steps=[], step),
            div(
              ~attr=Attr.classes(["stepper-justification"]),
              step.chosen_step
              |> Option.map((chosen_step: EvaluatorStep.step) =>
                   chosen_step.knd |> Stepper.get_justification |> Node.text
                 )
              |> Option.to_list,
            ),
          ],
        ),
      ]
      @ hidden_steps;
    };
    (
      (
        settings.core.evaluation.stepper_history
          ? List.map(previous_step(~hidden=false), tl)
            |> List.flatten
            |> List.rev_append(
                 _,
                 settings.core.evaluation.show_hidden_steps
                   ? hd
                     |> Stepper.hidden_steps_of_info
                     |> List.map(previous_step(~hidden=true))
                     |> List.flatten
                   : [],
               )
          : []
      )
      @ [current]
    )
    @ (
      settings.core.evaluation.show_settings
        ? SettingsModal.view(~inject, settings.core.evaluation) : []
    );
  };
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
    stepper_view(
      ~inject,
      ~settings,
      ~font_metrics,
      ~result_key,
      ~read_only=false,
      s,
    )
  };

let editor_view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~mousedown_updates: list(Update.t)=[],
      ~selected: option(editor_selection),
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
  let inject: CodeEditor.event => Ui_effect.t(unit) =
    if (locked) {
      _ => Effect.(Many([Prevent_default, Stop_propagation]));
    } else {
      fun
      | MakeActive => Effect.Many(List.map(inject, mousedown_updates))
      | MouseUp => inject(Update.SetMeta(Mouseup))
      | DragTo(point) =>
        inject(Update.PerformAction(Select(Resize(Goal(Point(point))))))
      | MouseDown(point) =>
        Effect.Many(
          List.map(
            inject,
            [Update.SetMeta(Mousedown)]
            @ mousedown_updates
            @ [PerformAction(Move(Goal(Point(point))))],
          ),
        )
      | JumpToBindingSiteOf(point) =>
        Effect.Many(
          List.map(
            inject,
            mousedown_updates
            @ [
              PerformAction(Move(Goal(Point(point)))),
              PerformAction(Jump(BindingSiteOfIndicatedVar)),
            ],
          ),
        )
      | DoubleClick => Update.PerformAction(Select(Tile(Current))) |> inject
      | TripleClick => Update.PerformAction(Select(Smart)) |> inject;
    };
  div(
    ~attr=
      Attr.classes([
        "cell",
        Option.is_some(selected) ? "selected" : "deselected",
        locked ? "locked" : "unlocked",
      ]),
    [
      CodeEditor.view(
        ~inject,
        ~ui_state,
        ~settings,
        ~selected=selected == Some(MainEditor),
        ~locked,
        ~caption,
        ~test_results,
        ~highlights,
        ~overlayer,
        ~error_ids,
        ~sort,
        editor,
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

let title_cell = title => {
  simple_cell_view([
    div(
      ~attr=Attr.class_("title-cell"),
      [div(~attr=Attr.class_("title-text"), [text(title)])],
    ),
  ]);
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
    ) => [
  editor_view(
    ~locked=true,
    ~selected=None,
    ~highlights,
    ~inject,
    ~ui_state,
    ~settings,
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
    ~selected=None,
    ~highlights=None,
    ~inject,
    ~ui_state,
    ~settings,
    ~footer,
    ~test_results=ModelResult.test_results(result),
    ~error_ids=statics.error_ids,
    editor,
  );
};
