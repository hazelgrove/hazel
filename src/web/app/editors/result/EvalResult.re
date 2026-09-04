open Util_web;
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
    cached_targets: Calc.saved(Sample.targets), /* Input targets for cache invalidation */
    result: Calc.t(ProgramResult.t(ProgramResult.inner)),
    dynamics: Calc.saved(option(Dynamics.t)),
    incr_eval: Calc.saved(EvaluatorState.incr_eval),
    /* ReusePass prediction for the current/last eval. Feeds the frozen debug
     * tint; kept after completion so fast evals remain inspectable. */
    predicted_reuse: EvaluatorState.incr_eval,
    streaming_outbox: Calc.saved(option(IncrEval.outbox(EvaluatorState.t))),
    streaming_state: Calc.saved(option(EvaluatorState.t)),
    pending_eval_ids: list(Id.t),
    display,
    theorems: Theorems.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    stepper: option(StepperView.Model.persistent),
    theorems: Theorems.Model.persistent,
  };

  let init = {
    cached_settings: Calc.Pending,
    elab: Calc.Pending,
    cached_targets: Calc.Pending,
    result: Calc.NewValue(ProgramResult.awaiting_worker_ack),
    dynamics: Calc.Pending,
    incr_eval: Calc.Pending,
    predicted_reuse: IncrEval.empty,
    streaming_outbox: Calc.Pending,
    streaming_state: Calc.Pending,
    pending_eval_ids: [],
    display: Evaluation(Calc.Pending),
    theorems: Theorems.Model.init,
  };

  let persist = (model: t): persistent => {
    stepper:
      switch (model.display) {
      | Stepper(stepper) => Some(StepperView.Model.persist(stepper))
      | _ => None
      },
    theorems: Theorems.Model.persist(model.theorems),
  };

  let unpersist = (p: persistent): t => {
    let theorems = Theorems.Model.unpersist(p.theorems);
    switch (p.stepper) {
    | Some(stepper) => {
        cached_settings: Calc.Pending,
        elab: Calc.Pending,
        cached_targets: Calc.Pending,
        result: Calc.NewValue(ProgramResult.awaiting_worker_ack),
        dynamics: Calc.Pending,
        incr_eval: Calc.Pending,
        predicted_reuse: IncrEval.empty,
        streaming_outbox: Calc.Pending,
        streaming_state: Calc.Pending,
        pending_eval_ids: [],
        display: Stepper(StepperView.Model.unpersist(stepper)),
        theorems,
      }
    | None => {
        ...init,
        theorems,
      }
    };
  };

  let probe_results = (model: t): option(Sample.Map.t) =>
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
    | None => Dynamics.Map.mk(Sample.Map.empty)
    };

  let predicted_reuse = (model: t): EvaluatorState.incr_eval =>
    model.predicted_reuse;

  let eval_is_pending = (model: t): bool =>
    switch (Calc.get_value(model.result)) {
    | ProgramResult.ResultPending(_) => true
    | ProgramResult.ResultOk(_)
    | ProgramResult.ResultFail(_) => false
    };

  let pending_eval_ids = (model: t): list(Id.t) =>
    eval_is_pending(model) ? model.pending_eval_ids : [];

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
    | UpdateResult(ProgramResult.t(ProgramResult.inner))
    | UpdateStreamingEval(IncrEval.outbox(EvaluatorState.t))
    | MergeStreamingEval(IncrEval.outbox(EvaluatorState.t))
    | TheoremsAction(Theorems.Update.t);

  // Update is meant to make minimal changes to the model, and calculate will do the rest.
  let update = (~settings, action, model: Model.t): Updated.t(Model.t) =>
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
    | (StepperAction(a), {display: Stepper(stepper), _}) =>
      let* stepper = StepperView.Update.update(~settings, a, stepper);
      {
        ...model,
        display: Stepper(stepper),
      };
    | (StepperAction(_), _) => model |> Updated.raise_invalid_action
    | (
        EvalEditorAction(a),
        {display: Evaluation(Calculated(Some((exp, editor)))), _},
      ) =>
      let* editor = CodeSelectable.Update.update(~settings, a, editor);
      {
        ...model,
        display: Evaluation(Calculated(Some((exp, editor)))),
      };
    | (EvalEditorAction(_), _) => model |> Updated.raise_invalid_action
    | (TheoremsAction(action), _) =>
      let* theorems =
        Theorems.Update.update(~settings, action, model.theorems);
      {
        ...model,
        theorems,
      };
    | (UpdateResult(result), _) =>
      {
        ...model,
        result: Calc.NewValue(result),
        pending_eval_ids:
          switch (result) {
          | ProgramResult.ResultPending(_) => model.pending_eval_ids
          | ProgramResult.ResultOk(_)
          | ProgramResult.ResultFail(_) => []
          },
      }
      |> Updated.return_quiet
    | (UpdateStreamingEval(stream), _) =>
      /* Worker ReusePlan arrives here (via on_ack). Snapshot it for the
       * frozen debug tint; also seed the streaming outbox / pending worklist. */
      {
        ...model,
        result: Calc.NewValue(ProgramResult.evaluating),
        predicted_reuse: stream.completed,
        streaming_outbox: Calc.Calculated(Some(stream)),
        streaming_state: Calc.Pending,
        pending_eval_ids:
          EvalWorklist.remove_streamed_ids(stream, model.pending_eval_ids),
      }
      |> Updated.return_quiet
    | (MergeStreamingEval(stream), _) =>
      let current =
        model.streaming_outbox
        |> Calc.get_saved(None)
        |> Option.value(~default=IncrEval.empty_outbox);
      {
        ...model,
        streaming_outbox:
          Calc.Calculated(Some(IncrEval.merge_outbox(stream, current))),
        streaming_state: Calc.Pending,
        pending_eval_ids:
          EvalWorklist.remove_streamed_ids(stream, model.pending_eval_ids),
      }
      |> Updated.return_quiet;
    };

  let calculate =
      (
        ~settings: CoreSettings.t,
        ~queue_worker: option(WorkerServer.Request.value => unit),
        ~is_edited: bool,
        statics: Haz3lcore.CachedStatics.t,
        {
          cached_settings,
          elab,
          cached_targets,
          result,
          dynamics,
          incr_eval,
          predicted_reuse,
          streaming_outbox,
          streaming_state,
          pending_eval_ids,
          display,
          theorems,
        }: Model.t,
      ) => {
    // Check whether settings / elab / targets have changed
    let settings =
      cached_settings
      |> Calc.set(settings, ~eq=CoreSettings.eq_ignoring_stepper_modals);
    let elab = Calc.set(~eq=Exp.fast_equal, statics.elaborated, elab);
    let targets =
      Calc.set(
        ~eq=Id.Map.equal(Sample.equal_capture_spec),
        statics.targets,
        cached_targets,
      );

    /* Previous incremental map, if the last evaluation produced one. Pull
     * from the saved field so it survives intermediate pending states
     * (during which `result` itself is ResultPending). */
    let prev_incr = incr_eval |> Calc.get_saved(IncrEval.empty);
    /* Project statics to the serializable slice the incremental evaluator
     * needs. The raw info_map can't cross postMessage because LivelitCtx
     * entries contain OCaml closures. */
    let eval_info_map =
      EvalInfo.of_info_map(
        ~probe_all=Calc.get_value(settings).probe_all,
        ~targets=Calc.get_value(targets),
        statics.info_map,
      );
    let result =
      result
      |> {
        let.calc_t elab = elab
        // TODO[Matt]: We could make this more fine-grained, we only care about one setting
        and.calc settings = settings
        and.calc _ = targets;
        switch (queue_worker) {
        // Dynamics is off:
        | _ when !settings.dynamics => ProgramResult.awaiting_worker_ack
        // Using the webworker:
        | Some(queue_worker) =>
          queue_worker({
            expr: elab,
            eval_info_map,
            prev: prev_incr,
          });
          ProgramResult.awaiting_worker_ack;
        // Using the main thread:
        | None =>
          switch (
            WorkerServer.evaluate_sync({
              expr: elab,
              eval_info_map,
              prev: prev_incr,
            })
          ) {
          | Ok((exp, state)) =>
            ProgramResult.ResultOk(
              ProgramResult.{
                result: exp,
                state,
              },
            )
          | Error(e) => ProgramResult.ResultFail(e)
          }
        };
      };

    let streaming_outbox =
      streaming_outbox
      |> {
        let.calc result = result;
        switch (result) {
        | ProgramResult.ResultPending(Evaluating) =>
          streaming_outbox |> Calc.get_saved(None)
        | ProgramResult.ResultPending(AwaitingWorkerAck)
        | ProgramResult.ResultFail(_)
        | ProgramResult.ResultOk(_) => None
        };
      };

    let pending_eval_ids =
      switch (result) {
      | NewValue(ProgramResult.ResultPending(AwaitingWorkerAck)) =>
        if (Calc.get_value(settings).dynamics) {
          switch (queue_worker) {
          | Some(_) => EvalWorklist.pending_ids(statics.info_map)
          | None => []
          };
        } else {
          [];
        }
      | NewValue(ProgramResult.ResultOk(_))
      | NewValue(ProgramResult.ResultFail(_)) => []
      | NewValue(ProgramResult.ResultPending(Evaluating))
      | OldValue(ProgramResult.ResultPending(_)) => pending_eval_ids
      | OldValue(ProgramResult.ResultOk(_))
      | OldValue(ProgramResult.ResultFail(_)) => []
      };

    /* Clear on a fresh eval request; ReusePlan / sync path re-fills it.
     * Otherwise keep the last prediction so the frozen tint stays useful
     * after a fast eval completes. */
    let predicted_reuse =
      switch (result, queue_worker) {
      | (NewValue(ProgramResult.ResultPending(AwaitingWorkerAck)), _) => IncrEval.empty
      | (NewValue(ProgramResult.ResultOk(_)), None) =>
        ReusePass.reuse_pass(
          ~prev=prev_incr,
          ~eval_info=eval_info_map,
          ~env=Builtins.env_init,
          Calc.get_value(elab),
        )
      | _ => predicted_reuse
      };

    let streaming_state =
      streaming_state
      |> {
        let.calc elab = elab
        and.calc streaming_outbox = streaming_outbox;
        switch (streaming_outbox) {
        | Some(streaming_outbox) =>
          Some(StreamCollector.collect_stream_state(streaming_outbox, elab))
        | None => None
        };
      };

    // Turn state into dynamics map
    let dynamics_of_state = (state: EvaluatorState.t) =>
      Dynamics.{
        probe_map: state |> EvaluatorState.get_probes,
        test_results:
          state |> EvaluatorState.get_tests |> TestResults.mk_results,
        theorems: state |> EvaluatorState.get_theorems,
      };
    let dynamics =
      dynamics
      |> {
        let.calc result = result
        and.calc streaming_state = streaming_state;
        switch (result, streaming_state) {
        | (ProgramResult.ResultPending(_), Some(state)) =>
          Some(dynamics_of_state(state))
        | (ProgramResult.ResultPending(_), None)
        | (ProgramResult.ResultFail(_), _) =>
          dynamics |> Calc.get_saved(None)
        | (ProgramResult.ResultOk({state, _}), _) =>
          Some(dynamics_of_state(state))
        };
      };

    let incr_eval =
      incr_eval
      |> {
        let.calc result = result
        and.calc streaming_outbox = streaming_outbox;
        switch (result, streaming_outbox) {
        | (ProgramResult.ResultPending(_), Some(streaming_outbox)) =>
          streaming_outbox.completed
        | (ProgramResult.ResultPending(_), None) =>
          incr_eval |> Calc.get_saved(IncrEval.empty)
        | (ProgramResult.ResultFail(_), _) => IncrEval.empty
        | (ProgramResult.ResultOk({state, _}), _) => state.incr_eval
        };
      };

    // Calculate the display
    let display =
      switch (display) {
      | Evaluation(ev_display) =>
        let ev_calc =
          ev_display
          |> {
            let.calc settings = settings
            and.calc result = result;
            switch (result) {
            | ResultOk({result: exp, _}) =>
              /* Evaluation always produces an Exp-sorted value (Drv-sorted
                 subterms only appear wrapped in DrvQuote, which is itself Exp),
                 so the result editor is rooted at Exp. */
              Some((
                exp,
                exp |> CodeSelectable.Model.mk_from_exp(~settings, ~root=Exp),
              ))
            | ResultFail(_)
            | ResultPending(_) =>
              ev_display |> Calc.get_saved_opt |> Option.join
            };
          };
        let result_changed = Calc.is_new(ev_calc);
        ev_calc
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
                   ~is_edited=is_edited || result_changed,
                   editor,
                 ),
               )
             ),
           )
        |> Calc.save
        |> (x => Model.Evaluation(x));
      | Stepper(stepper) =>
        Model.Stepper(
          StepperView.Update.calculate(
            ~settings,
            ~ctx=
              OldValue(
                SemanticCtx.of_ctx_and_env(
                  Builtins.ctx_init(None),
                  Builtins.closure_env,
                ),
              ),
            elab,
            stepper,
          ),
        )
      };

    // HACK[Matt]: say that statics is updated iff dynamics is updated
    let statics: Calc.t('a) =
      switch (dynamics) {
      | NewValue(_) => NewValue(statics)
      | OldValue(_) => OldValue(statics)
      };

    let theorems =
      Calc.get_value(settings).dynamics
        ? theorems
          |> Theorems.Update.calculate(~settings, ~statics, ~dynamics)
        : theorems;

    (
      {
        cached_settings: settings |> Calc.save,
        elab: elab |> Calc.save,
        cached_targets: targets |> Calc.save,
        result: result |> Calc.make_old,
        dynamics: dynamics |> Calc.save,
        incr_eval: incr_eval |> Calc.save,
        predicted_reuse,
        streaming_outbox: streaming_outbox |> Calc.save,
        streaming_state: streaming_state |> Calc.save,
        pending_eval_ids,
        display,
        theorems,
      }: Model.t
    );
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Evaluation(CodeSelectable.Selection.t)
    | Stepper(StepperView.Focus.t)
    | Theorems(Theorems.Focus.t);

  let get_cursor_info =
      (~inject, ~selection: t, mr: Model.t): cursor(Update.t) =>
    switch (selection, mr.display) {
    | (Evaluation(selection), Evaluation(Calculated(Some((_, editor))))) =>
      let+ ci =
        CodeSelectable.Selection.get_cursor_info(
          ~inject=x => inject(Update.EvalEditorAction(x)),
          ~selection,
          editor,
        );
      Update.EvalEditorAction(ci);
    | (Stepper(focus), Stepper(s)) =>
      let+ ci =
        StepperView.Focus.get_cursor_info(
          ~inject=x => inject(Update.StepperAction(x)),
          ~focus,
          s,
        );
      Update.StepperAction(ci);
    | (Evaluation(_), _) => Cursor.empty
    | (Stepper(_), _) => Cursor.empty
    | (Theorems(focus), _) =>
      let+ ci =
        Theorems.Focus.get_cursor_info(
          ~inject=x => inject(Update.TheoremsAction(x)),
          ~focus,
          mr.theorems,
        );
      Update.TheoremsAction(ci);
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

  let result_status_of: ProgramResult.t('a) => string =
    fun
    | ResultPending(_) => "pending"
    | ResultOk(_) => "ok"
    | ResultFail(_) => "fail";

  let status_classes_of: ProgramResult.t('a) => list(string) =
    fun
    | ResultPending(AwaitingWorkerAck) => ["pending", "pending-ack"]
    | ResultPending(Evaluating) => ["pending", "pending-evaluating"]
    | ResultOk(_) => ["ok"]
    | ResultFail(_) => ["fail"];

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
    let editor = Option.map(snd, editor);
    let code_view =
      Option.map(
        (editor: CodeSelectable.Model.t) =>
          CodeSelectable.View.view(
            ~signal=
              fun
              | MakeActive => signal(MakeActive(Evaluation())),
            ~edit_mode=
              EditMode.Editable({
                inject: a => inject(EvalEditorAction(a)),
                escape: _ => Ui_effect.Ignore,
                take_focus: _ => Ui_effect.Ignore,
                focus: selected ? Some() : None,
              }),
            ~globals,
            ~dynamics=editor.dynamics,
            editor,
          ),
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
            ~attrs=[Attr.classes(["status"] @ status_classes_of(result))],
            [
              div(~attrs=[Attr.classes(["spinner"])], []),
              div(~attrs=[Attr.classes(["eq"])], [text("≡")]),
            ],
          ),
          div(
            ~attrs=[Attr.classes(["result", result_status_of(result)])],
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
          ~selected=selected == Some(Evaluation()),
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
           | `NoTheorems
           | `JustTheorems
           | `Custom(Node.t)
         ]=`EvalResults,
        ~locked: bool,
        model: Model.t,
      ) =>
    switch (result_kind) {
    // Normal case:
    | `EvalResults
    | `NoTheorems
    | `JustTheorems when globals.settings.core.dynamics =>
      let result =
        result_kind == `JustTheorems
          ? [] : footer(~globals, ~signal, ~inject, ~selected, ~locked, model);
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
      let theorems =
        result_kind == `NoTheorems
          ? []
          : Theorems.View.view(
              ~globals,
              ~take_focus=f => signal(MakeActive(Theorems(f))),
              ~inject=a => inject(TheoremsAction(a)),
              ~selected=
                switch (selected) {
                | Some(Theorems(f)) => Some(f)
                | _ => None
                },
              model.theorems,
            );
      let theorems =
        List.length(theorems) == 0
          ? [] : [WebUtil.div_c("theorems", theorems)];
      (result @ theorems, test_overlay);

    // Just showing elaboration because evaluation is off:
    | `EvalResults
    | `NoTheorems when globals.settings.core.elaborate =>
      let result = [
        text("Evaluation disabled, showing elaboration:"),
        switch (Model.get_elaboration(model)) {
        | Some(elab) =>
          elab
          |> Haz3lcore.ExpToSegment.(
               exp_to_segment(
                 ~settings=
                   Settings.of_core(~inline=false, globals.settings.core),
               )
             )
          |> Haz3lcore.PrettySegment.prettify
          |> CodeViewable.view_segment(~globals)
        | None => text("No elaboration found")
        },
      ];
      (result, (_ => []));

    // Not showing any results:
    | `EvalResults
    | `NoTheorems
    | `JustTheorems
    | `NoResults => ([], (_ => []))

    | `Custom(node) => (
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
    | `TestResults =>
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
