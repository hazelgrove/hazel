open Util;
open Calc.Syntax;
open Language;

/* The result box at the bottom of a cell. This is either the TestResutls
   kind where only a summary of test results is shown, or the EvalResults kind
   where users can choose whether they want to use a single-stepper or see the
   result of full evaluation. */

/* This file follows conventions in [docs/ui-architecture.md] */

/* The cell's stepper is the theorem stepper over a hidden editor holding
 * `theorem cell = ? proof <steps> in ?`. Its proof is real syntax, so
 * picking a step patches it, its sub-editors splice into it, and the
 * checker checks it, as in a theorem. Its goal is the cell's elaborated
 * program rather than the statement. */
module CellStepper = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    goal: Exp.t,
    editor: CodeEditable.Model.t,
    /* The hidden editor changed since its proof was last checked. */
    edited: bool,
    proof_map: ProofMap.t,
    stepper: StepperView.Model.t,
  };

  let init = (elab: Exp.t): t => {
    let zipper =
      Haz3lcore.Parser.to_zipper(~root=Exp, "theorem cell = ? proof ? in ?")
      |> Option.value(~default=Haz3lcore.Zipper.init());
    {
      goal:
        elab |> Substitution.in_exp(Builtins.env_init) |> Exp.replace_all_ids,
      editor:
        CodeEditable.Model.mk(Haz3lcore.Editor.Model.mk(~root=Exp, zipper)),
      edited: true,
      proof_map: ProofMap.empty,
      stepper: StepperView.Model.init,
    };
  };

  let ctx =
    SemanticCtx.of_ctx_and_env(
      Builtins.ctx_init(None),
      Builtins.closure_env,
    );

  let theorem_proof = (e: Exp.t): option(Proof.t) =>
    switch (Exp.term_of(e)) {
    | Theorem(_, _, proof, _) => Some(proof)
    | _ => None
    };

  /* The proof as written. */
  let proof = (cell: t): Proof.t =>
    CodeEditable.Model.get_statics(cell.editor).term
    |> theorem_proof
    |> Option.value(~default=Proof.fresh(EmptyHole));

  let calculate = (~settings: CoreSettings.t, cell: t): t =>
    if (!cell.edited) {
      cell;
    } else {
      let editor =
        CodeEditable.Update.calculate(
          ~settings,
          ~is_edited=true,
          ~stitch=x => x,
          ~dynamics=Dynamics.Map.empty,
          ~is_dynamic_term=false,
          cell.editor,
        );
      let statics = CodeEditable.Model.get_statics(editor);
      /* The checker checks the elaborated proof, which keeps the written
       * proof's ids. */
      let proof_map =
        switch (
          Option.bind(
            Exp.find_by_id(Exp.rep_id(statics.term), statics.elaborated),
            theorem_proof,
          )
        ) {
        | Some(elaborated) =>
          Evaluator.Proofs.check_proof(
            ~statics=statics.info_map,
            ~ctx,
            ~goal=cell.goal,
            elaborated,
          )
        | None => ProofMap.empty
        };
      let cell = {
        ...cell,
        editor,
        edited: false,
        proof_map,
      };
      {
        ...cell,
        stepper:
          StepperView.Update.calculate(
            ~settings,
            StepperRows.chain(~proof_map, proof(cell)),
            cell.stepper,
          ),
      };
    };
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type display =
    | Evaluation(Calc.saved(option((Exp.t, CodeSelectable.Model.t))))
    | Stepper(CellStepper.t);

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
    /* Bumped whenever an input of evaluation (elab, settings, targets)
     * changes. Worker replies carry the generation of their request, and
     * replies to an older request are refused. */
    request_gen: int,
    /* The statics the latest request was built from. */
    request_statics: option(Haz3lcore.CachedStatics.t),
    /* The latest completed evaluation and the statics of its request,
     * which Theorems checks proofs with. While a newer request is pending,
     * or after it fails, the pair stays, so a proof is never checked
     * against one program's statics and another's environment. */
    checked: Calc.saved(option((Haz3lcore.CachedStatics.t, Dynamics.t))),
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
    request_gen: 0,
    request_statics: None,
    checked: Calc.Pending,
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

  /* Proof-check results for this cell, which Theorems computes on the
   * main thread with the real statics (empty until calculated, or if the
   * cell contains no theorems). Consumed by the cursor inspector to show
   * incoming/outgoing expressions on proof sub-terms. */
  let proof_map = (model: t): ProofMap.t => model.theorems.proof_map;

  let request_gen = (model: t): int => model.request_gen;

  let get_elaboration = (model: t): option(Exp.t) =>
    model.elab |> Calc.get_saved_opt;

  /* The evaluated value, however the model came to hold it. `UpdateResult`
     announces a FRESH evaluation, which is the right signal for work that
     should happen once per evaluation; anything that has to track the value
     itself must read it here instead, because a model restored by undo
     carries a result no `UpdateResult` will ever be sent for. */
  let get_value = (model: t): option(Exp.t) =>
    switch (Calc.get_value(model.result)) {
    | ProgramResult.ResultOk({result, _}) => Some(result)
    | ProgramResult.ResultFail(_)
    | ProgramResult.ResultPending(_) => None
    };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleStepper
    | StepperAction(StepperView.Update.t)
    /* Edits to the cell stepper's hidden editor: a picked step, or typing
     * in one of its sub-editors. */
    | StepperPatch(Haz3lcore.EditorTransform.patch)
    | StepperEditorAction(CodeEditable.Update.t)
    | EvalEditorAction(CodeSelectable.Update.t)
    /* Worker replies carry the generation of the request they answer. */
    | UpdateResult(int, ProgramResult.t(ProgramResult.inner))
    | UpdateStreamingEval(int, IncrEval.outbox(EvaluatorState.t))
    | MergeStreamingEval(int, IncrEval.outbox(EvaluatorState.t))
    | TheoremsAction(Theorems.Update.t);

  /* Whether a cell the user can't edit still takes this action: evaluation
     results arrive as these whether or not the cell is editable. Exhaustive on
     purpose, so a new action has to be classified here. */
  let applies_to_read_only = (action: t): bool =>
    switch (action) {
    | UpdateResult(_)
    | UpdateStreamingEval(_)
    | MergeStreamingEval(_) => true
    | ToggleStepper
    | StepperAction(_)
    | StepperPatch(_)
    | StepperEditorAction(_)
    | EvalEditorAction(_)
    | TheoremsAction(_) => false
    };

  // Update is meant to make minimal changes to the model, and calculate will do the rest.
  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action, model) {
    /* Toggling the stepper and its patches, from clicks in the stepper,
     * leave the caret where it was, so they don't take the view back to
     * it. */
    | (ToggleStepper, {display: Stepper(_), _}) =>
      {
        ...model,
        display: Evaluation(Calc.Pending),
      }
      |> Updated.return(~scroll=IfCaretVisible)
    | (ToggleStepper, {display: Evaluation(_), _}) =>
      let elab = model.elab |> Calc.get_saved(Exp.fresh(EmptyHole));
      {
        ...model,
        display: Stepper(CellStepper.init(elab)),
      }
      |> Updated.return(~scroll=IfCaretVisible);
    | (StepperAction(a), {display: Stepper(cell), _}) =>
      let* stepper = StepperView.Update.update(~settings, a, cell.stepper);
      {
        ...model,
        display:
          Stepper({
            ...cell,
            stepper,
          }),
      };
    | (StepperPatch(patch), {display: Stepper(cell), _}) =>
      let zipper =
        Haz3lcore.EditorTransform.apply_patch(
          cell.editor.editor.state.zipper,
          patch,
        );
      {
        ...model,
        display:
          Stepper({
            ...cell,
            editor: {
              ...cell.editor,
              editor: Haz3lcore.Editor.Model.mk(~root=Exp, zipper),
            },
            edited: true,
          }),
      }
      |> Updated.return(~scroll=IfCaretVisible);
    | (StepperEditorAction(a), {display: Stepper(cell), _}) =>
      let* editor = CodeEditable.Update.update(~settings, a, cell.editor);
      {
        ...model,
        display:
          Stepper({
            ...cell,
            editor,
            edited: true,
          }),
      };
    | (StepperAction(_) | StepperPatch(_) | StepperEditorAction(_), _) =>
      model |> Updated.raise_invalid_action
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
    | (UpdateResult(gen, _), _)
    | (UpdateStreamingEval(gen, _), _)
    | (MergeStreamingEval(gen, _), _) when gen != model.request_gen =>
      model |> Updated.return_quiet
    | (UpdateResult(_, result), _) =>
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
    | (UpdateStreamingEval(_, stream), _) =>
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
    | (MergeStreamingEval(_, stream), _) =>
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
          request_gen,
          request_statics,
          checked,
        }: Model.t,
      ) => {
    // Check whether settings / elab / targets have changed
    let settings =
      cached_settings
      |> Calc.set(settings, ~eq=CoreSettings.eq_ignoring_stepper_modals);
    let elab =
      Calc.set(~eq=Exp.fast_equal_with_lexemes, statics.elaborated, elab);
    let targets =
      Calc.set(
        ~eq=Id.Map.equal(Sample.equal_capture_spec),
        statics.targets,
        cached_targets,
      );

    /* A new request whenever `result` below is recomputed. */
    let new_request =
      Calc.is_new(elab) || Calc.is_new(settings) || Calc.is_new(targets);
    let request_gen = new_request ? request_gen + 1 : request_gen;
    let request_statics = new_request ? Some(statics) : request_statics;

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
            generation: request_gen,
          });
          ProgramResult.awaiting_worker_ack;
        // Using the main thread:
        | None =>
          switch (
            WorkerServer.evaluate_sync({
              expr: elab,
              eval_info_map,
              prev: prev_incr,
              generation: request_gen,
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
        probe_map: state |> EvaluatorState.get_probes |> Sample.Map.finalize,
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
        | (ProgramResult.ResultPending(_), None) =>
          dynamics |> Calc.get_saved(None)
        | (ProgramResult.ResultFail(_), _) =>
          dynamics |> Calc.get_saved(None)
        | (ProgramResult.ResultOk({state, _}), _) =>
          Some(dynamics_of_state(state))
        };
      };

    let checked =
      switch (result, request_statics, checked) {
      | (NewValue(ProgramResult.ResultOk({state, _})), Some(statics), _) =>
        Calc.NewValue(Some((statics, dynamics_of_state(state))))
      | (_, _, Calculated(checked)) => Calc.OldValue(checked)
      | (_, _, Pending) => Calc.NewValue(None)
      };

    let incr_eval =
      incr_eval
      |> {
        let.calc result = result
        and.calc streaming_outbox = streaming_outbox;
        switch (result, streaming_outbox) {
        | (ProgramResult.ResultPending(_), Some(streaming_outbox)) => {
            ...streaming_outbox.completed,
            steps: Calc.get_saved(IncrEval.empty, incr_eval).steps,
          }
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
      /* A new program starts its proof over. */
      | Stepper(cell) =>
        let cell =
          Calc.is_new(elab) ? CellStepper.init(Calc.get_value(elab)) : cell;
        Model.Stepper(
          CellStepper.calculate(~settings=Calc.get_value(settings), cell),
        );
      };

    let theorems =
      Calc.get_value(settings).dynamics
        ? Theorems.Update.calculate(
            ~settings,
            ~statics=Calc.map(_ => statics, elab),
            ~checked,
            theorems,
          )
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
        request_gen,
        request_statics,
        checked: checked |> Calc.save,
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
    | (Stepper(focus), Stepper(cell)) =>
      let+ ci =
        StepperView.Focus.get_cursor_info(
          ~inject=x => inject(Update.StepperAction(x)),
          ~focus,
          cell.stepper,
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
    | Stepper(cell) =>
      let stepper_view =
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
          ~is_toplevel=true,
          ~edit_syntax=patch => inject(StepperPatch(patch)),
          ~backing={
            model: cell.editor,
            inject: a => inject(StepperEditorAction(a)),
          },
          ~info_map=CodeEditable.Model.get_statics(cell.editor).info_map,
          ~proof_map=cell.proof_map,
          ~exits=_ => [],
          CellStepper.proof(cell),
          cell.stepper,
        );
      /* The stepper renders from the last proof map / elaboration it saw,
       * so on evaluation failure it silently shows stale rows; surface the
       * failure above them. */
      switch (model.result |> Calc.get_value) {
      | ResultFail(err) => [
          div(
            ~attrs=[
              Attr.classes(["cell-item", "cell-result", "stepper-stale"]),
            ],
            [
              div(
                ~attrs=[Attr.classes(["error-msg"])],
                [text(error_msg(err))],
              ),
              div(
                ~attrs=[Attr.classes(["stale-note"])],
                [text("Steps below show the last successful evaluation.")],
              ),
            ],
          ),
          ...stepper_view,
        ]
      | _ => stepper_view
      };
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
           | `TestSigilsOnly
           | `TestResults
           | `EvalResults
           | `NoTheorems
           | `JustTheorems
           | `Custom(Node.t)
         ]=`EvalResults,
        ~locked: bool,
        /* Writes a patch into the cell's program. */
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        /* The cell's editor, which theorem steppers splice into. */
        ~main_editor: CodeEditable.Channel.t,
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
              ~measured=Haz3lcore.CachedSyntax.measured(editor.syntax),
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
              ~edit_syntax,
              ~main_editor,
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

    // test forms keep pass/fail sigils but no result footer (tutorial mode)
    | `TestSigilsOnly when globals.settings.core.dynamics =>
      let test_overlay = (editor: Haz3lcore.Editor.t) =>
        switch (Model.test_results(model)) {
        | Some(result) => [
            test_result_layer(
              ~font_metrics=globals.font_metrics,
              ~measured=Haz3lcore.CachedSyntax.measured(editor.syntax),
              result,
            ),
          ]
        | None => []
        };
      ([], test_overlay);

    // Not showing any results:
    | `EvalResults
    | `NoTheorems
    | `JustTheorems
    | `TestSigilsOnly
    | `NoResults => ([], (_ => []))

    | `Custom(node) => (
        [node],
        (
          (editor: Haz3lcore.Editor.t) =>
            switch (Model.test_results(model)) {
            | Some(result) => [
                test_result_layer(
                  ~font_metrics=globals.font_metrics,
                  ~measured=Haz3lcore.CachedSyntax.measured(editor.syntax),
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
              ~measured=Haz3lcore.CachedSyntax.measured(editor.syntax),
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
