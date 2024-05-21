open Haz3lcore;
open Virtual_dom.Vdom;
open Sexplib.Std;
open Node;
open Util;

type model = ModelResult.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | StepperEditorAction(int, Action.t)
  | StepperAction(UpdateAction.stepper_action)
  | ToggleStepper;

let update = (~settings: CoreSettings.t, action, model) =>
  switch (action) {
  | StepperEditorAction(idx, action) =>
    ModelResult.perform(~settings, idx, action, model)
  | StepperAction(stepper_action) =>
    switch (stepper_action) {
    | UpdateAction.StepForward(idx) => ModelResult.step_forward(idx, model)
    | UpdateAction.StepBackward =>
      ModelResult.step_backward(~settings=settings.evaluation, model)
    | UpdateAction.HideStepper => ModelResult.toggle_stepper(~settings, model)
    }
  | ToggleStepper => ModelResult.toggle_stepper(~settings, model)
  };

module View = {
  type event =
    | MakeActive(Editors.Selection.cell)
    | MouseUp
    | MouseDown;

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

  let stepper_view =
      (
        ~signal as _: event => Ui_effect.t(unit),
        ~inject: action => Ui_effect.t(unit),
        ~inject_global: Update.t => Ui_effect.t(unit),
        ~ui_state,
        ~settings: Settings.t,
        ~read_only: bool,
        stepper: Stepper.t,
      ) => {
    let history =
      Stepper.get_history(~settings=settings.core.evaluation, stepper);
    switch (history) {
    | [] => []
    | [hd, ..._] =>
      let button_back =
        Widgets.button_d(
          Icons.undo,
          inject(StepperAction(StepBackward)),
          ~disabled=
            !Stepper.can_undo(~settings=settings.core.evaluation, stepper),
          ~tooltip="Step Backwards",
        );
      let button_hide_stepper =
        Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
          inject(StepperAction(HideStepper))
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
      let (statics, editor, _) = hd.hidden_history |> Aba.hd;
      let current_model: StepperEditor.Steppable.model = {
        editor: {
          editor,
          statics,
        },
        next_steps:
          List.map(
            (option: EvaluatorStep.EvalObj.t) => option.d_loc |> Exp.rep_id,
            stepper.next_options,
          ),
      };
      let current =
        div(
          ~attr=Attr.classes(["cell-item", "cell-result"]),
          read_only
            ? [
              div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
              StepperEditor.Steppable.view(
                // TODO: Maybe get rid of this signal?
                ~signal=
                  (TakeStep(x)) => inject(StepperAction(StepForward(x))),
                ~ui_state,
                ~settings,
                ~overlays=[],
                current_model,
              ),
            ]
            : [
              div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
              StepperEditor.Steppable.view(
                ~signal=
                  (TakeStep(x)) => inject(StepperAction(StepForward(x))),
                ~ui_state,
                ~settings,
                ~overlays=[],
                current_model,
              ),
              button_back,
              eval_settings,
              toggle_show_history,
              button_hide_stepper,
            ],
        );
      let previous_step =
          (~hidden: bool, ~idx as _, step: Stepper.step_info): list(Node.t) => {
        let (statics, editor, _) = hd.hidden_history |> Aba.hd;
        let _ = "One" |> print_endline;
        let model: StepperEditor.Stepped.model = {
          editor: {
            editor,
            statics,
          },
          step: step.chosen_step |> Option.get, // TODO[Matt]: refactor away Option.get
          step_id: (step.chosen_step |> Option.get).d_loc |> Exp.rep_id,
        };
        let _ = "Two" |> print_endline;
        [
          div(
            ~attr=
              Attr.classes(
                ["cell-item", "cell-result"] @ (hidden ? ["hidden"] : []),
              ),
            [
              div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
              StepperEditor.Stepped.view(
                ~ui_state,
                ~settings,
                ~overlays=[],
                model,
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
                  history,
                )
                |> List.flatten
                |> List.rev
                |> List.tl
                |> List.rev;
              List.mapi(
                (idx, (hidden, step)) =>
                  previous_step(~hidden, ~idx=idx + 1, step),
                steps |> List.rev,
              )
              |> List.flatten;
            }
            : []
        )
        @ [current]
      )
      @ (
        settings.core.evaluation.show_settings
          ? SettingsModal.view(
              ~inject=inject_global,
              settings.core.evaluation,
            )
          : []
      );
    };
  };

  let live_eval =
      (
        ~signal: event => Ui_effect.t(unit),
        ~inject: action => Ui_effect.t(unit),
        ~ui_state,
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
    let code_view =
      CodeEditor.view(
        ~signal=
          fun
          | MakeActive => signal(MakeActive(MainEditor))
          | MouseUp => signal(MouseUp)
          | MouseDown => signal(MouseDown),
        ~inject=a => inject(StepperEditorAction(0, a)),
        ~ui_state,
        ~settings,
        ~selected,
        ~highlights=None,
        ~sort=Sort.root,
        {editor, statics: CachedStatics.empty_statics},
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
        ]
        @ (
          locked
            ? []
            : [
              Widgets.toggle(~tooltip="Show Stepper", "s", false, _ =>
                inject(ToggleStepper)
              ),
            ]
        ),
      )
    );
  };

  let footer =
      (
        ~signal,
        ~inject,
        ~inject_global,
        ~ui_state: Model.ui_state,
        ~settings: Settings.t,
        ~result: ModelResult.t,
        ~selected: option(int),
        ~locked,
      ) =>
    switch (result) {
    | _ when !settings.core.dynamics => []
    | NoElab => []
    | Evaluation(result) => [
        live_eval(
          ~signal,
          ~inject,
          ~ui_state,
          ~settings,
          ~selected=selected == Some(0),
          ~locked,
          result,
        ),
      ]
    | Stepper(s) =>
      stepper_view(
        ~signal,
        ~inject,
        ~inject_global,
        ~settings,
        ~ui_state,
        ~read_only=locked,
        s,
      )
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

  type result_kind =
    | NoResults
    | TestResults
    | EvalResults;

  let view =
      (
        ~signal: event => Ui_effect.t(unit),
        ~inject: action => Ui_effect.t(unit),
        ~inject_global: UpdateAction.t => Ui_effect.t(unit),
        ~ui_state: Model.ui_state,
        ~settings: Settings.t,
        ~selected: bool,
        ~result_kind=EvalResults,
        ~locked: bool,
        model: model,
      ) =>
    switch (result_kind) {
    // Normal case:
    | EvalResults when settings.core.dynamics =>
      let result =
        footer(
          ~signal,
          ~inject,
          ~inject_global,
          ~ui_state: Model.ui_state,
          ~settings,
          ~result=model,
          ~selected=selected ? Some(0) : None,
          ~locked,
        );
      let test_overlay = (editor: Editor.t) =>
        switch (ModelResult.test_results(model)) {
        | Some(result) =>
          test_result_layer(
            ~font_metrics=ui_state.font_metrics,
            ~measured=editor.state.meta.measured,
            result,
          )
        | None => []
        };
      (result, test_overlay);

    // Just showing elaboration because evaluation is off:
    | EvalResults when settings.core.elaborate =>
      let result = [
        text("Evaluation disabled, showing elaboration:"),
        switch (ModelResult.get_elaboration(model)) {
        | Some(elab) =>
          ReadOnlyEditor.view(
            ~ui_state,
            ~settings,
            {
              editor: elab.d |> ExpToSegment.exp_to_editor(~inline=false),
              statics: CachedStatics.empty_statics,
            }: CodeEditor.model,
          )
        | None => text("No elaboration found")
        },
      ];
      (result, (_ => []));

    // Not showing any results:
    | EvalResults
    | NoResults => ([], (_ => []))

    // Just showing test results (school mode)
    | TestResults =>
      let test_results = ModelResult.test_results(model);
      let test_overlay = (editor: Editor.t) =>
        switch (ModelResult.test_results(model)) {
        | Some(result) =>
          test_result_layer(
            ~font_metrics=ui_state.font_metrics,
            ~measured=editor.state.meta.measured,
            result,
          )
        | None => []
        };
      (
        [
          Cell.report_footer_view([
            TestView.test_summary(~inject=inject_global, ~test_results),
          ]),
        ],
        test_overlay,
      );
    };
};

let view = View.view;
