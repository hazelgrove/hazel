open Util;
open Calc.Syntax;
open Language;

/* The result box at the bottom of a cell. This is either the TestResutls
   kind where only a summary of test results is shown, or the EvalResults kind
   where users can choose whether they want to use a single-stepper or see the
   result of full evaluation. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type display =
    | Evaluation(Calc.saved(option((Exp.t, CodeSelectable.Model.t))))
    | Stepper(StepperView.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cached_settings: Calc.saved(CoreSettings.t),
    elab: Calc.saved(Exp.t),
    result: Calc.t(ProgramResult.t(ProgramResult.inner)),
    dynamics: Calc.saved(option(Dynamics.t)),
    display,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {stepper: option(StepperView.Model.persistent)};

  let init = {
    cached_settings: Calc.Pending,
    elab: Calc.Pending,
    result: Calc.NewValue(ProgramResult.ResultPending),
    dynamics: Calc.Pending,
    display: Evaluation(Calc.Pending),
  };

  let persist = (model: t): persistent => {
    stepper:
      switch (model.display) {
      | Stepper(stepper) => Some(StepperView.Model.persist(stepper))
      | _ => None
      },
  };

  let unpersist = (p: persistent): t => {
    switch (p.stepper) {
    | Some(stepper) => {
        cached_settings: Calc.Pending,
        elab: Calc.Pending,
        result: Calc.NewValue(ProgramResult.ResultPending),
        dynamics: Calc.Pending,
        display: Stepper(StepperView.Model.unpersist(stepper)),
      }
    | None => init
    };
  };

  let probe_results = (model: t): option(Dynamics.Probe.Map.t) =>
    model.dynamics
    |> Calc.get_saved(None)
    |> Option.map((d: Dynamics.t) => d.probe_map);

  let test_results = (model: t): option(TestResults.t) =>
    model.dynamics
    |> Calc.get_saved(None)
    |> Option.map((d: Dynamics.t) => d.test_results);

  let dynamics = (model: t): Dynamics.Map.t =>
    switch (probe_results(model)) {
    | Some(dynamics_map) => Dynamics.Map.mk(dynamics_map)
    | None => Dynamics.Map.mk(Dynamics.Probe.Map.empty)
    };

  let get_elaboration = (model: t): option(Exp.t) =>
    model.elab |> Calc.get_saved_opt;
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
  let update = (~globals, action, model: Model.t): Updated.t(Model.t) =>
    switch (action, model) {
    | (ToggleStepper, {display: Stepper(_), _}) =>
      {
        ...model,
        display: Evaluation(Calc.Pending),
      }
      |> Updated.return
    | (ToggleStepper, {display: Evaluation(_), _}) =>
      {
        ...model,
        display: Stepper(StepperView.Model.init),
      }
      |> Updated.return
<<<<<<< HEAD
    | (StepperAction(a), {result: Stepper(s), _}) =>
      let* stepper = StepperView.Update.update(~globals, a, s);
=======
    | (StepperAction(a), {display: Stepper(stepper), _}) =>
      let* stepper = StepperView.Update.update(~settings, a, stepper);
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
      {
        ...model,
        display: Stepper(stepper),
      };
    | (StepperAction(_), _) => model |> Updated.return_quiet
    | (
        EvalEditorAction(a),
        {display: Evaluation(Calculated(Some((exp, editor)))), _},
      ) =>
      let* editor =
        CodeSelectable.Update.update(
          ~dynamics=Dynamics.Map.empty,
          ~globals,
          a,
          editor,
        );
      {
        ...model,
        display: Evaluation(Calculated(Some((exp, editor)))),
      };
    | (EvalEditorAction(_), _) => model |> Updated.return_quiet
    | (UpdateResult(result), _) =>
      {
        ...model,
        result: Calc.NewValue(result),
      }
      |> Updated.return_quiet
    };

  let calculate =
      (
        ~globals,
        ~settings: CoreSettings.t,
        ~queue_worker: option(Language.Exp.t => unit),
        statics: Haz3lcore.CachedStatics.t,
        {cached_settings, elab, result, dynamics, display}: Model.t,
      ) => {
    // Check whether settings / elab have changed
    let settings =
      cached_settings
      |> Calc.set(settings, ~eq=CoreSettings.eq_ignoring_stepper_modals);
    let elab = Calc.set(~eq=Exp.fast_equal, statics.elaborated, elab);

    // Calculate the result
    let result =
      result
      |> {
        let.calc_t elab = elab
        and.calc settings = settings; // TODO[Matt]: We could make this more fine-grained, we only care about one setting
        switch (queue_worker) {
        // Dynamics is off:
        | _ when !settings.dynamics => ProgramResult.ResultPending
        // Using the webworker:
        | Some(queue_worker) =>
          queue_worker(elab);
<<<<<<< HEAD
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
        let s' = StepperView.Update.calculate(~globals, elab, s);
        {
          ...model,
          result: Stepper(s'),
        };
      | (Stepper, _) =>
        let s =
          StepperView.Model.init
          |> StepperView.Update.calculate(~globals, elab);
        {
          ...model,
          result: Stepper(s),
=======
          ProgramResult.ResultPending;
        // Using the main thread:
        | None =>
          switch (WorkerServer.work(elab)) {
          | Ok((exp, state)) =>
            ProgramResult.ResultOk(
              ProgramResult.{
                result: exp,
                state,
              },
            )
          | Error(e) => ProgramResult.ResultFail(e)
          }
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
        };
      };

    // Turn state into dynamics map
    let dynamics =
      dynamics
      |> {
        let.calc result = result;
        switch (result) {
        | ProgramResult.ResultPending => dynamics |> Calc.get_saved(None)
        | ProgramResult.ResultFail(_) => dynamics |> Calc.get_saved(None)
        | ProgramResult.ResultOk({state, _}) =>
          Some(
            Dynamics.{
              probe_map: state |> EvaluatorState.get_probes,
              test_results:
                state |> EvaluatorState.get_tests |> TestResults.mk_results,
            },
          )
        };
      };

    // Calculate the display
    let display =
      switch (display) {
      | Evaluation(ev_display) =>
        ev_display
        |> {
<<<<<<< HEAD
          let.calc _ = cached_settings
          and.calc result = result;
          switch (result) {
          | ResultOk((exp, _state)) =>
            Exp(exp)
            |> CodeSelectable.Model.mk_uncalculated(~inline=false)
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
               ~common=Globals.to_common_global(globals),
               ~is_dynamic_term=true,
               ~stitch=_ => exp,
               ~dynamics=Model.dynamics(model),
               editor,
             )
             |> (x => (exp, x))
=======
          let.calc settings = settings
          and.calc result = result;
          switch (result) {
          | ResultOk({result: exp, _}) =>
            Some((exp, exp |> CodeSelectable.Model.mk_from_exp(~settings)))
          | ResultFail(_)
          | ResultPending => ev_display |> Calc.get_saved_opt |> Option.join
          };
        }
        |> Calc.make_new  // TODO[Matt]: Could eventually replace this by keeping track of whether the editor selection has changed
        |> Calc.map_if_new(
             Option.map(((exp, editor)) =>
               (
                 exp,
                 CodeSelectable.Update.calculate(
                   ~settings=settings |> Calc.get_value,
                   ~is_dynamic_term=true,
                   ~stitch=_ => exp,
                   ~dynamics=Dynamics.Map.empty,
                   ~is_edited,
                   editor,
                 ),
               )
             ),
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
           )
        |> Calc.save
        |> (x => Model.Evaluation(x))
      | Stepper(stepper) =>
        Model.Stepper(StepperView.Update.calculate(~settings, elab, stepper))
      };

    (
      {
        cached_settings: settings |> Calc.save,
        elab: elab |> Calc.save,
        result: result |> Calc.make_old,
        dynamics: dynamics |> Calc.save,
        display,
      }: Model.t
    );
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Evaluation(CodeSelectable.Selection.t)
    | Stepper(StepperView.Focus.t);

<<<<<<< HEAD
  let get_cursor_info =
      (~globals, ~inject, ~selection: t, mr: Model.t): Haz3lcore.Cursor.t =>
    switch (selection, mr.result) {
    | (_, NoElab) => Haz3lcore.Cursor.empty
    | (Evaluation(selection), Evaluation({editor: Calculated(editor), _})) =>
      CodeSelectable.Selection.get_cursor_info(
        ~common=Globals.to_common_global(globals),
        ~inject=x => inject(Update.EvalEditorAction(x)),
        ~dynamics=Dynamics.Map.empty,
        editor |> snd,
        selection,
      )
    | (Stepper(selection), Stepper(s)) =>
      StepperView.Selection.get_cursor_info(
        ~globals,
        ~inject=x => inject(Update.StepperAction(x)),
        ~selection,
        s,
      )
    | (_, Evaluation(_)) => Haz3lcore.Cursor.empty
    | (_, Stepper(_)) => Haz3lcore.Cursor.empty
=======
  let get_cursor_info = (~selection: t, mr: Model.t): cursor(Update.t) =>
    switch (selection, mr.display) {
    | (Evaluation(selection), Evaluation(Calculated(Some((_, editor))))) =>
      let+ ci = CodeSelectable.Selection.get_cursor_info(~selection, editor);
      Update.EvalEditorAction(ci);
    | (Stepper(focus), Stepper(s)) =>
      let+ ci = StepperView.Focus.get_cursor_info(~focus, s);
      Update.StepperAction(ci);
    | (_, Evaluation(_)) => empty
    | (_, Stepper(_)) => empty
    };

  let handle_key_event =
      (~selection: t, ~event, mr: Model.t): option(Update.t) =>
    switch (selection, mr.display) {
    | (Evaluation(selection), Evaluation(Calculated(Some((_, editor))))) =>
      CodeSelectable.Selection.handle_key_event(~selection, editor, event)
      |> Option.map(x => Update.EvalEditorAction(x))
    | (Stepper(focus), Stepper(s)) =>
      StepperView.Focus.handle_key_event(~focus, s, ~event)
      |> Option.map(x => Update.StepperAction(x))
    | (_, Evaluation(_)) => None
    | (_, Stepper(_)) => None
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
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
    | ResultFail(_) => "fail";

  let live_eval =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected,
        ~locked,
        result: ProgramResult.t(ProgramResult.inner),
        editor: option(('a, CodeSelectable.Model.t)),
      ) => {
<<<<<<< HEAD
    let editor =
      switch (editor) {
      | Calculated(editor) => editor |> snd
      | _ =>
        Exp(elab)
        |> CodeSelectable.Model.mk_uncalculated(~inline=false)
        |> CodeSelectable.Update.calculate(
             ~common=Globals.to_common_global(globals),
             ~is_dynamic_term=true,
             ~stitch=_ => elab,
             ~dynamics=Dynamics.Map.empty,
           )
      };
    let code_view =
      CodeSelectable.View.view(
        ~take_focus=f => signal(MakeActive(Evaluation(f))),
        ~inject=a => inject(EvalEditorAction(a)),
        ~escape=_ => Ui_effect.Ignore,
        ~common={
          settings: globals.settings.core,
          font_metrics: globals.font_metrics,
          secondary_icons: globals.settings.secondary_icons,
          color_highlights: globals.color_highlights,
          statics: editor |> Haz3lcore.EditorManager.Model.get_statics,
          dynamics: Dynamics.Map.empty,
        },
        ~focus=selected,
        ~sort=Haz3lcore.Sort.root,
        editor.editor,
=======
    let editor = Option.map(snd, editor);
    let code_view =
      Option.map(
        CodeSelectable.View.view(
          ~signal=
            fun
            | MakeActive => signal(MakeActive(Evaluation())),
          ~inject=a => inject(EvalEditorAction(a)),
          ~globals,
          ~selected,
          ~sort=Sort.root,
        ),
        editor,
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
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
            Option.to_list(code_view),
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
        ~selected: option(Selection.t),
        ~locked,
        model: Model.t,
      ) =>
    switch (model.display) {
    | _ when !globals.settings.core.dynamics => []
    | Evaluation(editor) => [
        live_eval(
          ~globals,
          ~signal,
          ~inject,
          ~selected=
            switch (selected) {
            | Some(Evaluation(s)) => Some(s)
            | _ => None
            },
          ~locked,
          model.result |> Calc.get_value,
          editor |> Calc.get_saved_exc(~print="result editor missing"),
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

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        ~result_kind: [
           | `NoResults
           | `TestResults
           | `EvalResults
           | `Custom(Node.t)
         ]=`EvalResults,
        ~locked: bool,
        model: Model.t,
      ) =>
    switch (result_kind) {
    // Normal case:
    | `EvalResults when globals.settings.core.dynamics =>
      let result =
<<<<<<< HEAD
        footer(~globals, ~signal, ~inject, ~result=model, ~selected, ~locked);
      let test_overlay = (editor: Haz3lcore.Editor.Model.t) =>
=======
        footer(~globals, ~signal, ~inject, ~selected, ~locked, model);
      let test_overlay = (editor: Haz3lcore.Editor.t) =>
>>>>>>> defc38690ed8035ae8950d148523ee0b25c22021
        switch (Model.test_results(model)) {
        | Some(result) => [
            test_result_layer(
              ~font_metrics=globals.font_metrics,
              ~measured=editor |> Haz3lcore.Editor.get_measured,
              result,
            ),
          ]
        | None => []
        };
      (result, test_overlay);

    // Just showing elaboration because evaluation is off:
    | `EvalResults when globals.settings.core.elaborate =>
      let result = [
        text("Evaluation disabled, showing elaboration:"),
        switch (Model.get_elaboration(model)) {
        | Some(elab) =>
          let shape_map = Util.ProjectorShape.Map.empty; // assume no projectors
          elab
          |> Haz3lcore.ExpToSegment.(
               exp_to_segment(
                 ~settings=
                   Settings.of_core(~inline=false, globals.settings.core),
               )
             )
          |> CodeViewable.view_segment(
               ~font_metrics=globals.font_metrics,
               ~secondary_icons=globals.settings.secondary_icons,
               ~sort=Exp,
               ~shape_map,
             );
        | None => text("No elaboration found")
        },
      ];
      (result, (_ => []));

    // Not showing any results:
    | `EvalResults
    | `NoResults => ([], (_ => []))

    | `Custom(node) => (
        [node],
        (
          (editor: Haz3lcore.Editor.Model.t) =>
            switch (Model.test_results(model)) {
            | Some(result) => [
                test_result_layer(
                  ~font_metrics=globals.font_metrics,
                  ~measured=editor |> Haz3lcore.Editor.get_measured,
                  result,
                ),
              ]
            | None => []
            }
        ),
      )

    // Just showing test results (school mode)
    | `TestResults =>
      let test_results = Model.test_results(model);
      let test_overlay = (editor: Haz3lcore.Editor.Model.t) =>
        switch (Model.test_results(model)) {
        | Some(result) => [
            test_result_layer(
              ~font_metrics=globals.font_metrics,
              ~measured=editor |> Haz3lcore.Editor.get_measured,
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
