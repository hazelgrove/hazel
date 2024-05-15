open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;

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
      ~select: Editors.cell_selection => Ui_effect.t(unit),
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~ui_state,
      ~result_key: string,
      ~settings: Settings.t,
      ~selected,
      ~locked,
      result: ModelResult.eval_result,
    ) => {
  let editor =
    switch (result.evaluation, result.previous) {
    | (ResultOk(res), _) => res.editor
    | (ResultPending, ResultOk(res)) => res.editor
    | _ => result.elab.d |> ExpToSegment.exp_to_editor(~inline=false)
    };
  let inject': CodeEditor.event => Ui_effect.t(unit) =
    fun
    | MouseUp => SetMeta(Mouseup) |> inject
    | DragTo(point) =>
      PerformAction(Select(Resize(Goal(Point(point))))) |> inject
    | MouseDown(point) =>
      Effect.Many([
        inject(SetMeta(Mousedown)),
        select(Result(0)),
        inject(PerformAction(Move(Goal(Point(point))))),
      ])
    | JumpToBindingSiteOf(point) =>
      Effect.Many([
        inject(PerformAction(Move(Goal(Point(point))))),
        select(Result(0)),
        inject(PerformAction(Jump(BindingSiteOfIndicatedVar))),
      ])
    | DoubleClick => PerformAction(Select(Tile(Current))) |> inject
    | TripleClick => PerformAction(Select(Smart)) |> inject;
  let code_view =
    CodeEditor.view(
      ~select=() => select(Result(0)),
      ~inject=inject',
      ~ui_state,
      ~settings,
      ~selected,
      ~locked,
      ~test_results=None,
      ~highlights=None,
      ~error_ids=[],
      ~overlayer=None,
      ~sort=Sort.root,
      editor,
    );
  let exn_view =
    switch (result.evaluation) {
    | ResultFail(err) => [
        div(~attr=Attr.classes(["error-msg"]), [text(error_msg(err))]),
      ]
    | _ => []
    };
  Node.(
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
          [code_view],
        ),
        Widgets.toggle(~tooltip="Show Stepper", "s", false, _ =>
          inject(UpdateAction.ToggleStepper(result_key))
        ),
      ],
    )
  );
};

let stepper_view =
    (
      ~select: Editors.cell_selection => Ui_effect.t(unit),
      ~inject: UpdateAction.stepper_action => Ui_effect.t(unit),
      ~inject_global: Update.t => Ui_effect.t(unit),
      ~ui_state,
      ~settings,
      ~read_only: bool,
      ~selected: option(int),
      stepper: Stepper.t,
    ) => {
  let step_dh_code =
      (
        ~next_steps as _,
        ~selected: bool,
        ~idx: int,
        {previous_step: _, hidden_history, chosen_step: _}: Stepper.step_info,
      ) => {
    let (_, editor, _) = Aba.hd(hidden_history);
    div(
      ~attr=Attr.classes(["result"]),
      [
        CodeEditor.view(
          ~select=() => select(Result(idx)),
          ~inject=
            fun
            | MouseUp => SetMeta(Mouseup) |> inject_global
            | DragTo(point) =>
              PerformAction(Select(Resize(Goal(Point(point)))))
              |> inject_global
            | MouseDown(point) =>
              Effect.Many([
                inject_global(SetMeta(Mousedown)),
                select(Result(idx)),
                inject_global(PerformAction(Move(Goal(Point(point))))),
              ])
            | JumpToBindingSiteOf(point) =>
              Effect.Many([
                inject_global(PerformAction(Move(Goal(Point(point))))),
                select(Result(idx)),
                inject_global(
                  PerformAction(Jump(BindingSiteOfIndicatedVar)),
                ),
              ])
            | DoubleClick =>
              PerformAction(Select(Tile(Current))) |> inject_global
            | TripleClick => PerformAction(Select(Smart)) |> inject_global,
          ~ui_state,
          ~settings,
          ~selected,
          ~locked=read_only,
          ~test_results=None,
          ~highlights=None,
          ~error_ids=[],
          ~overlayer=None,
          ~sort=Sort.root,
          editor,
        ),
      ],
    );
  };
  let history =
    Stepper.get_history(~settings=settings.core.evaluation, stepper);
  switch (history) {
  | [] => []
  | [hd, ...tl] =>
    let button_back =
      Widgets.button_d(
        Icons.undo,
        inject(StepBackward),
        ~disabled=
          !Stepper.can_undo(~settings=settings.core.evaluation, stepper),
        ~tooltip="Step Backwards",
      );
    let button_hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
        inject(HideStepper)
      );
    let toggle_show_history =
      Widgets.toggle(
        ~tooltip="Show History",
        "h",
        settings.core.evaluation.stepper_history,
        _ =>
        inject_global(Set(Evaluation(ShowRecord)))
      );
    let eval_settings =
      Widgets.button(Icons.gear, _ =>
        inject_global(Set(Evaluation(ShowSettings)))
      );
    let current =
      div(
        ~attr=Attr.classes(["cell-item", "cell-result"]),
        read_only
          ? [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            step_dh_code(
              ~next_steps=[],
              ~idx=0,
              ~selected=selected == Some(0),
              hd,
            ),
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
              ~idx=0,
              ~selected=selected == Some(0),
              hd,
            ),
            button_back,
            eval_settings,
            toggle_show_history,
            button_hide_stepper,
          ],
      );
    let dh_code_previous = step_dh_code;
    let previous_step =
        (~hidden: bool, ~idx, step: Stepper.step_info): list(Node.t) => {
      [
        div(
          ~attr=
            Attr.classes(
              ["cell-item", "cell-result"] @ (hidden ? ["hidden"] : []),
            ),
          [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            dh_code_previous(
              ~next_steps=[],
              ~selected=Some(idx) == selected,
              ~idx,
              step,
            ),
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
      ];
    };
    (
      (
        settings.core.evaluation.stepper_history
          ? {
            let steps: list((bool, Stepper.step_info)) =
              List.map(
                step =>
                  [
                    (false, step),
                    ...settings.core.evaluation.show_hidden_steps
                         ? Stepper.hidden_steps_of_info(step)
                           |> List.map(x => (true, x))
                         : [],
                  ],
                tl,
              )
              |> List.flatten;
            List.mapi(
              (idx, (hidden, step)) =>
                previous_step(~hidden, ~idx=idx + 1, step),
              steps,
            )
            |> List.flatten;
          }
          : []
      )
      @ [current]
    )
    @ (
      settings.core.evaluation.show_settings
        ? SettingsModal.view(~inject=inject_global, settings.core.evaluation)
        : []
    );
  };
};

let footer =
    (
      ~locked,
      ~select,
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~result: ModelResult.t,
      ~selected: option(int),
      ~result_key,
    ) =>
  switch (result) {
  | _ when !settings.core.dynamics => []
  | NoElab => []
  | Evaluation(result) => [
      live_eval(
        ~locked,
        ~select,
        ~inject,
        ~ui_state,
        ~settings,
        ~result_key,
        ~selected=selected == Some(0),
        result,
      ),
    ]
  | Stepper(s) =>
    stepper_view(
      ~select,
      ~inject=
        stepper_action =>
          inject(UpdateAction.StepperAction(result_key, stepper_action)),
      ~inject_global=inject,
      ~settings,
      ~ui_state,
      ~read_only=false,
      ~selected,
      s,
    )
  };

let editor_view =
    (
      ~select,
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~selected: option(Editors.cell_selection),
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
      | MouseUp => inject(Update.SetMeta(Mouseup))
      | DragTo(point) =>
        inject(Update.PerformAction(Select(Resize(Goal(Point(point))))))
      | MouseDown(point) =>
        Effect.Many([
          inject(Update.SetMeta(Mousedown)),
          select(Editors.MainEditor),
          inject(Update.PerformAction(Move(Goal(Point(point))))),
        ])
      | JumpToBindingSiteOf(point) =>
        Effect.Many([
          select(Editors.MainEditor),
          inject(Update.PerformAction(Move(Goal(Point(point))))),
          inject(Update.PerformAction(Jump(BindingSiteOfIndicatedVar))),
        ])
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
    Option.to_list(caption)
    @ [
      CodeEditor.view(
        ~select=() => select(Editors.MainEditor),
        ~inject,
        ~ui_state,
        ~settings,
        ~selected=selected == Some(MainEditor),
        ~locked,
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
      ~select,
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
    ~select,
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
      ~select,
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
          ~select,
          ~inject,
          ~settings,
          ~ui_state,
          ~result_key=target_id,
          ~result,
          ~selected=None,
        )
      : [];
  editor_view(
    ~select,
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
