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
    | JumpTo(Haz3lcore.Id.t);

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
        ~globals as {settings, inject_global, _} as globals: Globals.t,
        ~signal as _: event => Ui_effect.t(unit),
        ~inject: action => Ui_effect.t(unit),
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
                ~globals,
                ~signal=
                  (TakeStep(x)) => inject(StepperAction(StepForward(x))),
                ~overlays=[],
                current_model,
              ),
            ]
            : [
              div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
              StepperEditor.Steppable.view(
                ~globals,
                ~signal=
                  (TakeStep(x)) => inject(StepperAction(StepForward(x))),
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
        let (statics, editor, _) = step.hidden_history |> Aba.hd;
        let model: StepperEditor.Stepped.model = {
          editor: {
            editor,
            statics,
          },
          step: step.chosen_step, // TODO[Matt]: refactor away Option.get
          step_id:
            step.chosen_step
            |> Option.map((x: EvaluatorStep.step) => x.d_loc |> Exp.rep_id),
        };
        [
          div(
            ~attr=
              Attr.classes(
                ["cell-item", "cell-result"] @ (hidden ? ["hidden"] : []),
              ),
            [
              div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
              StepperEditor.Stepped.view(~globals, ~overlays=[], model),
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
                |> List.tl;
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
              ~inject=u => inject_global(Set(u)),
              settings.core.evaluation,
            )
          : []
      );
    };
  };

  let live_eval =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: action => Ui_effect.t(unit),
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
          | MakeActive => signal(MakeActive(Result(0))),
        ~inject=a => inject(StepperEditorAction(0, a)),
        ~globals,
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
        ~globals: Globals.t,
        ~signal,
        ~inject,
        ~result: ModelResult.t,
        ~selected: option(int),
        ~locked,
      ) =>
    switch (result) {
    | _ when !globals.settings.core.dynamics => []
    | NoElab => []
    | Evaluation(result) => [
        live_eval(
          ~globals,
          ~signal,
          ~inject,
          ~selected=selected == Some(0),
          ~locked,
          result,
        ),
      ]
    | Stepper(s) =>
      stepper_view(~globals, ~signal, ~inject, ~read_only=locked, s)
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
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: action => Ui_effect.t(unit),
        ~selected: bool,
        ~result_kind=EvalResults,
        ~locked: bool,
        model: model,
      ) =>
    switch (result_kind) {
    // Normal case:
    | EvalResults when globals.settings.core.dynamics =>
      let result =
        footer(
          ~globals,
          ~signal,
          ~inject,
          ~result=model,
          ~selected=selected ? Some(0) : None,
          ~locked,
        );
      let test_overlay = (editor: Editor.t) =>
        switch (ModelResult.test_results(model)) {
        | Some(result) =>
          test_result_layer(
            ~font_metrics=globals.font_metrics,
            ~measured=editor.state.meta.measured,
            result,
          )
        | None => []
        };
      (result, test_overlay);

    // Just showing elaboration because evaluation is off:
    | EvalResults when globals.settings.core.elaborate =>
      let result = [
        text("Evaluation disabled, showing elaboration:"),
        switch (ModelResult.get_elaboration(model)) {
        | Some(elab) =>
          ReadOnlyEditor.view(
            ~globals,
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
            ~font_metrics=globals.font_metrics,
            ~measured=editor.state.meta.measured,
            result,
          )
        | None => []
        };
      (
        [
          Cell.report_footer_view([
            TestView.test_summary(
              ~inject_jump=tile => signal(JumpTo(tile)),
              ~test_results,
            ),
          ]),
        ],
        test_overlay,
      );
    };
};

let view = View.view;
