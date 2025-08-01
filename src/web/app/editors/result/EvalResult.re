open Util;
open Language;

/* The result box at the bottom of a cell. This is either the TestResutls
   kind where only a summary of test results is shown, or the EvalResults kind
   where users can choose whether they want to use a single-stepper or see the
   result of full evaluation. */

/* This file follows conventions in [docs/ui-architecture.md] */

module type Model = {
  type t;
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type result =
    | NoElab
    | Evaluation({
        elab: Exp.t,
        result: Calc.t(ProgramResult.t((Exp.t, EvaluatorState.t))),
        cached_settings: Calc.saved(CoreSettings.t),
        editor: Calc.saved((Exp.t, CodeSelectable.Model.t)),
      })
    | Stepper(StepperView.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind =
    | Evaluation
    | Stepper;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    kind,
    result,
    previous_tests: option(TestResults.t), // Stops test results from being cleared on update
    previous_probes: option(Dynamics.Probe.Map.t) // As above
  };

  let init = {
    kind: Evaluation,
    result: NoElab,
    previous_tests: None,
    previous_probes: None,
  };

  let eval_state = (result: result): option(EvaluatorState.t) =>
    switch (result) {
    | Evaluation(e) =>
      switch (e.result) {
      | OldValue(ResultOk((_, state)))
      | NewValue(ResultOk((_, state))) => Some(state)
      | OldValue(ResultFail(_) | ResultPending | Off(_))
      | NewValue(ResultFail(_) | ResultPending | Off(_)) => None
      }
    | Stepper(s) => Some(s |> StepperView.Model.get_state)
    | NoElab => None
    };

  let probe_results = (model: t): option(Dynamics.Probe.Map.t) =>
    switch (eval_state(model.result)) {
    | None => model.previous_probes
    | Some(eval_state) => Some(EvaluatorState.get_probes(eval_state))
    };

  let test_results = (model: t): option(TestResults.t) =>
    switch (eval_state(model.result)) {
    | None => model.previous_tests
    | Some(eval_state) => Some(TestResults.of_state(eval_state))
    };

  let make_test_report = (model: t): option(TestResults.t) =>
    switch (eval_state(model.result)) {
    | None => None
    | Some(_) => test_results(model)
    };

  let dynamics = (model: t): Dynamics.Map.t =>
    switch (probe_results(model)) {
    | Some(dynamics_map) => Dynamics.Map.mk(dynamics_map)
    | None => Dynamics.Map.mk(Dynamics.Probe.Map.empty)
    };

  let get_elaboration = (model: t): option(Exp.t) =>
    switch (model.result) {
    | Evaluation({elab, _}) => Some(elab)
    | Stepper(s) => StepperView.Model.get_elaboration(s)
    | _ => None
    };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleStepper
    | StepperAction(StepperView.Update.t)
    | EvalEditorAction(CodeSelectable.Update.t)
    | UpdateResult(ProgramResult.t(ProgramResult.inner));

  let can_undo = (action: t) => {
    switch (action) {
    | ToggleStepper => true
    | StepperAction(action) => StepperView.Update.can_undo(action)
    | EvalEditorAction(action) => CodeSelectable.Update.can_undo(action)
    | UpdateResult(_) => false
    };
  };

  // Update is meant to make minimal changes to the model, and calculate will do the rest.
  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action, model) {
    | (ToggleStepper, {kind: Stepper, _}) =>
      {
        ...model,
        kind: Evaluation,
      }
      |> Updated.return
    | (ToggleStepper, {kind: Evaluation, _}) =>
      {
        ...model,
        kind: Stepper,
      }
      |> Updated.return
    | (StepperAction(a), {result: Stepper(s), _}) =>
      let* stepper = StepperView.Update.update(~settings, a, s);
      {
        ...model,
        result: Stepper(stepper),
      };
    | (StepperAction(_), _) => model |> Updated.return_quiet
    | (
        EvalEditorAction(a),
        {
          result:
            Evaluation({
              elab,
              result,
              cached_settings,
              editor: Calculated((exp, editor)),
            }),
          _,
        },
      ) =>
      let* editor = CodeSelectable.Update.update(~settings, a, editor);
      {
        ...model,
        result:
          Evaluation({
            elab,
            result,
            cached_settings,
            editor: Calculated((exp, editor)),
          }),
      };
    | (EvalEditorAction(_), _) => model |> Updated.return_quiet
    | (
        UpdateResult(update),
        {result: Evaluation({elab, editor, cached_settings, _}), _},
      ) =>
      {
        ...model,
        result:
          Evaluation({
            elab,
            result:
              NewValue(
                ProgramResult.map(
                  ({result: exp, state: s}: ProgramResult.inner) => {
                    (exp, s)
                  },
                  update,
                ),
              ),
            editor,
            cached_settings,
          }),
      }
      |> (
        x => {
          ...x,
          previous_tests: Model.test_results(x),
          previous_probes: Model.probe_results(x),
        }
      )
      |> Updated.return
    | (UpdateResult(_), _) => model |> Updated.return_quiet
    };
  };

  let calculate =
      (
        ~settings: CoreSettings.t,
        ~queue_worker: option(Exp.t => unit),
        ~is_edited: bool,
        statics: Haz3lcore.CachedStatics.t,
        model: Model.t,
      ) => {
    let elab = statics.elaborated;
    let model =
      switch (model.kind, model.result) {
      // If elab hasn't changed, don't recalculate
      | (
          Evaluation,
          Evaluation({elab: elab', result, cached_settings, editor}),
        )
          when Exp.fast_equal(elab, elab') => {
          ...model,
          result:
            Evaluation({
              elab,
              result,
              cached_settings,
              editor,
            }),
        }
      // If elab has changed, recalculate
      | (Evaluation, _) when settings.dynamics =>
        switch (queue_worker) {
        | None => {
            ...model,
            result:
              Evaluation({
                elab,
                result: {
                  switch (WorkerServer.work(elab)) {
                  | Ok((exp, state)) =>
                    NewValue(ProgramResult.ResultOk((exp, state)))
                  | Error(e) => NewValue(ProgramResult.ResultFail(e))
                  };
                },
                cached_settings: Pending,
                editor: Pending,
              }),
          }

        | Some(queue_worker) =>
          queue_worker(elab);
          {
            ...model,
            result:
              Evaluation({
                elab,
                result: NewValue(ProgramResult.ResultPending),
                cached_settings: Pending,
                editor: Pending,
              }),
          };
        }
      | (Evaluation, _) => {
          ...model,
          result: NoElab,
        }
      | (Stepper, Stepper(s)) =>
        let s' = StepperView.Update.calculate(~settings, elab, s);
        {
          ...model,
          result: Stepper(s'),
        };
      | (Stepper, _) =>
        let s =
          StepperView.Model.init
          |> StepperView.Update.calculate(~settings, elab);
        {
          ...model,
          result: Stepper(s),
        };
      };

    // Calculate evaluation editor
    switch (model.result) {
    | Evaluation({elab, result, cached_settings, editor}) =>
      open Calc.Syntax;
      let cached_settings = Calc.set(~eq=(==), settings, cached_settings);
      let editor =
        editor
        |> Calc.map_saved(x => Calc.Calculated(x))
        |> {
          let.calc settings = cached_settings
          and.calc result = result;
          switch (result) {
          | ResultOk((exp, _state)) =>
            exp
            |> CodeSelectable.Model.mk_from_exp(~settings)
            |> (x => Calc.Calculated((exp, x)))
          | ResultFail(_) => Pending
          | ResultPending => Pending
          | Off(_) => Pending
          };
        };
      let editor =
        editor
        |> Calc.get_value
        |> Calc.map_saved(((exp, editor)) =>
             CodeSelectable.Update.calculate(
               ~settings,
               ~is_dynamic_term=true,
               ~stitch=_ => exp,
               ~is_edited,
               ~dynamics=Model.dynamics(model),
               editor,
             )
             |> (x => (exp, x))
           )
        |> (x => Calc.OldValue(x));
      {
        ...model,
        result:
          Evaluation({
            elab,
            result: Calc.make_old(result),
            cached_settings: Calc.save(cached_settings),
            editor: Calc.get_value(editor),
          }),
      };
    | _ => model
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Evaluation(CodeSelectable.Selection.t)
    | Stepper(StepperView.Focus.t);

  let get_cursor_info = (~selection: t, mr: Model.t): cursor(Update.t) =>
    switch (selection, mr.result) {
    | (_, NoElab) => empty
    | (Evaluation(selection), Evaluation({editor: Calculated(editor), _})) =>
      let+ ci =
        CodeSelectable.Selection.get_cursor_info(~selection, editor |> snd);
      Update.EvalEditorAction(ci);
    | (Stepper(focus), Stepper(s)) =>
      let+ ci = StepperView.Focus.get_cursor_info(~focus, s);
      Update.StepperAction(ci);
    | (_, Evaluation(_)) => empty
    | (_, Stepper(_)) => empty
    };

  let handle_key_event =
      (~selection: t, ~event, mr: Model.t): option(Update.t) =>
    switch (selection, mr.result) {
    | (_, NoElab) => None
    | (Evaluation(selection), Evaluation({editor: Calculated(editor), _})) =>
      CodeSelectable.Selection.handle_key_event(
        ~selection,
        editor |> snd,
        event,
      )
      |> Option.map(x => Update.EvalEditorAction(x))
    | (Stepper(focus), Stepper(s)) =>
      StepperView.Focus.handle_key_event(~focus, s, ~event)
      |> Option.map(x => Update.StepperAction(x))
    | (_, Evaluation(_)) => None
    | (_, Stepper(_)) => None
    };
};

module View = {
  open Virtual_dom.Vdom;
  open WebUtil.Node;

  type event =
    | MakeActive(Selection.t)
    | JumpTo(Id.t);

  let error_msg = (err: ProgramResult.error) =>
    switch (err) {
    | EvaulatorError(err) => EvaluatorError.show(err)
    | UnknownException(str) => str
    | Timeout => "Evaluation timed out"
    };

  let status_of: ProgramResult.t('a) => string =
    fun
    | ResultPending => "pending"
    | ResultOk(_) => "ok"
    | ResultFail(_) => "fail"
    | Off(_) => "off";

  let live_eval =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected,
        ~locked,
        elab: Exp.t,
        result: ProgramResult.t((Exp.t, EvaluatorState.t)),
        editor: Calc.saved(('a, CodeSelectable.Model.t)),
      ) => {
    let editor =
      switch (editor) {
      | Calculated(editor) => editor |> snd
      | _ =>
        elab
        |> CodeSelectable.Model.mk_from_exp(~settings=globals.settings.core)
      };
    let code_view =
      CodeSelectable.View.view(
        ~signal=
          fun
          | MakeActive => signal(MakeActive(Evaluation())),
        ~inject=a => inject(EvalEditorAction(a)),
        ~globals,
        ~selected,
        ~sort=Sort.root,
        editor,
      );
    let exn_view =
      switch (result) {
      | ResultFail(err) => [
          div(
            ~attrs=[Attr.classes(["error-msg"])],
            [text(error_msg(err))],
          ),
        ]
      | _ => []
      };
    Node.(
      div(
        ~attrs=[Attr.classes(["cell-item", "cell-result"])],
        exn_view
        @ [
          div(
            ~attrs=[Attr.classes(["status", status_of(result)])],
            [
              div(~attrs=[Attr.classes(["spinner"])], []),
              div(~attrs=[Attr.classes(["eq"])], [text("≡")]),
            ],
          ),
          div(
            ~attrs=[Attr.classes(["result", status_of(result)])],
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
        ~result: Model.t,
        ~selected: option(Selection.t),
        ~locked,
      ) =>
    switch (result.result) {
    | _ when !globals.settings.core.dynamics => []
    | NoElab => []
    | Evaluation({elab, result, editor, _}) => [
        live_eval(
          ~globals,
          ~signal,
          ~inject,
          ~selected=selected == Some(Evaluation()),
          ~locked,
          elab,
          result |> Calc.get_value,
          editor,
        ),
      ]
    | Stepper(s) =>
      StepperView.View.view(
        ~globals,
        ~selected=
          switch (selected) {
          | Some(Stepper(s)) => Some(s)
          | _ => None
          },
        ~signal=
          fun
          | HideStepper => inject(ToggleStepper)
          | MakeActive(s) => signal(MakeActive(Stepper(s))),
        ~inject=x => inject(StepperAction(x)),
        s,
      )
    };

  let test_status_icon_view =
      (~font_metrics, insts, ms: Haz3lcore.Measured.Shards.t): option(Node.t) =>
    switch (ms) {
    | [(_, {origin: _, last}), ..._] =>
      let status = insts |> TestMap.joint_status |> TestStatus.to_string;
      let pos = DecUtil.abs_position(~font_metrics, last);
      Some(
        Node.div(~attrs=[Attr.classes(["test-result", status]), pos], []),
      );
    | _ => None
    };

  let test_result_layer =
      (
        ~font_metrics,
        ~measured: Haz3lcore.Measured.t,
        test_results: TestResults.t,
      )
      : WebUtil.Node.t =>
    WebUtil.div_c(
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

  type result_kind =
    | NoResults
    | TestResults
    | EvalResults
    | Custom(Node.t);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        ~result_kind=EvalResults,
        ~locked: bool,
        model: Model.t,
      ) =>
    switch (result_kind) {
    // Normal case:
    | EvalResults when globals.settings.core.dynamics =>
      let result =
        footer(~globals, ~signal, ~inject, ~result=model, ~selected, ~locked);
      let test_overlay = (editor: Haz3lcore.Editor.t) =>
        switch (Model.test_results(model)) {
        | Some(result) => [
            test_result_layer(
              ~font_metrics=globals.font_metrics,
              ~measured=editor.syntax.measured,
              result,
            ),
          ]
        | None => []
        };
      (result, test_overlay);

    // Just showing elaboration because evaluation is off:
    | EvalResults when globals.settings.core.elaborate =>
      let result = [
        text("Evaluation disabled, showing elaboration:"),
        switch (Model.get_elaboration(model)) {
        | Some(elab) =>
          let shape_map = Haz3lcore.ProjectorCore.Shape.Map.empty; // assume no projectors
          elab
          |> Haz3lcore.ExpToSegment.(
               exp_to_segment(
                 ~settings=
                   Settings.of_core(~inline=false, globals.settings.core),
               )
             )
          |> CodeViewable.view_segment(~globals, ~sort=Exp, ~shape_map);
        | None => text("No elaboration found")
        },
      ];
      (result, (_ => []));

    // Not showing any results:
    | EvalResults
    | NoResults => ([], (_ => []))

    | Custom(node) => (
        [node],
        (
          (editor: Haz3lcore.Editor.t) =>
            switch (Model.test_results(model)) {
            | Some(result) => [
                test_result_layer(
                  ~font_metrics=globals.font_metrics,
                  ~measured=editor.syntax.measured,
                  result,
                ),
              ]
            | None => []
            }
        ),
      )

    // Just showing test results (school mode)
    | TestResults =>
      let test_results = Model.test_results(model);
      let test_overlay = (editor: Haz3lcore.Editor.t) =>
        switch (Model.test_results(model)) {
        | Some(result) => [
            test_result_layer(
              ~font_metrics=globals.font_metrics,
              ~measured=editor.syntax.measured,
              result,
            ),
          ]
        | None => []
        };
      (
        [
          CellCommon.report_footer_view([
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
