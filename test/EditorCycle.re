open Web;
open Haz3lcore;
open Language;

/* Drive the editor's own update cycle in-process, so a test can ask for the
 * term the editor WOULD HAVE SENT to the worker, and then run it the way the
 * worker runs it.
 *
 * Every other level of projector testing reconstructs the pipeline by hand:
 * parse, `Statics.mk`, `evaluate`. That is not what the editor does. The
 * editor's elaboration comes from `CachedStatics` via
 * `CodeWithStatics.Update.calculate`, and its evaluation is sliced by
 * `WorkerServer`. A projector that commits syntax is only verified end to end
 * once both of those are in the loop — see docs/testing-projectors.md. */

let settings = CoreSettings.on;

/* The pass the editor runs after every edit. `is_edited` is what forces a
   statics recompute rather than reusing the previous `CachedStatics`. */
let calculate =
    (~is_edited: bool, model: CodeWithStatics.Model.t)
    : CodeWithStatics.Model.t =>
  CodeWithStatics.Update.calculate(
    ~settings,
    ~is_edited,
    ~stitch=x => x,
    ~dynamics=model.dynamics,
    ~is_dynamic_term=false,
    model,
  );

/* An editor model over program text, with statics calculated the editor's way. */
let of_text = (~root=Sort.Exp, text: string): CodeWithStatics.Model.t => {
  let z =
    switch (Parser.to_zipper(~root, text)) {
    | Some(z) => z
    | None => failwith("EditorCycle.of_text: could not parse")
    };
  Editor.Model.mk(z, ~root)
  |> CodeWithStatics.Model.mk
  |> calculate(~is_edited=true);
};

/* Perform an editor action and re-run the pass, as the update cycle does. */
let perform =
    (a: Action.t, model: CodeWithStatics.Model.t)
    : result(CodeWithStatics.Model.t, string) =>
  switch (
    Perform.go(
      ~settings,
      ~statics=model.statics,
      ~syntax=model.editor.syntax,
      ~root=model.editor.root,
      a,
      {
        zipper: model.editor.state.zipper,
        col_target: None,
      },
    )
  ) {
  | Error(f) => Error(Action.Failure.show(f))
  | Ok(zipper) =>
    let editor = Editor.Model.mk(zipper, ~root=model.editor.root);
    Ok(
      calculate(
        ~is_edited=true,
        {
          ...model,
          editor,
        },
      ),
    );
  };

/* What the editor posts to the worker: the elaborated term from CachedStatics
   plus the eval_info EvalResult.Update.calculate builds from the info_map --
   `of_info_map`, NOT `of_targets`. The difference is not cosmetic: only
   `of_info_map` populates the per-id `statics` field (elab_term, co_ctx,
   probe_targets), which is a real evaluator input. A harness that passes
   `of_targets` is evaluating a different request than the editor does. */
let request = (model: CodeWithStatics.Model.t): (Exp.t, EvalInfo.t) => (
  model.statics.elaborated,
  EvalInfo.of_info_map(
    ~probe_all=settings.probe_all,
    ~targets=model.statics.targets,
    model.statics.info_map,
  ),
);

/* Evaluate that request the way WorkerServer does: in slices, resuming a
   continuation, at the worker's own step budget. */
let evaluate_as_worker =
    (
      ~step_budget: int=5000,
      ~max_slices: int=40000,
      ~prev=IncrEval.empty,
      model: CodeWithStatics.Model.t,
    )
    : result((Exp.t, EvaluatorState.t), string) => {
  let (expr, eval_info) = request(model);
  let rec drive = (n, evaluation) =>
    n > max_slices
      ? Error(
          "did not complete in " ++ string_of_int(max_slices) ++ " slices",
        )
      : (
        switch (Evaluator.run_yielding_slice(~step_budget, evaluation)) {
        | EvaluationCompleted(pair) => Ok(pair)
        | EvaluationYielded(evaluation) => drive(n + 1, evaluation)
        }
      );
  drive(
    0,
    Evaluator.start_yielding_evaluation(
      ~prev,
      ~eval_info,
      ~env=Builtins.env_init,
      expr,
    ),
  );
};

/* The incremental map a *pending* evaluation leaves behind.
   `EvalResult.Update.calculate` sets `incr_eval := streaming_outbox.completed`
   whenever the result is still `ResultPending`, and that value is what goes
   out as `prev` on the next request. When the worker abandons a run (a newer
   request arrives mid-flight, `WorkerServer.is_latest`), this partial map is
   the `prev` the next evaluation reuses -- so it is a real input to the
   evaluator, not just an intermediate. Drive `slices` slices and harvest it. */
let partial_prev =
    (
      ~step_budget: int=5000,
      ~slices: int,
      ~prev=IncrEval.empty,
      model: CodeWithStatics.Model.t,
    )
    : EvaluatorState.incr_eval => {
  let (expr, eval_info) = request(model);
  let rec drive = (n, evaluation) =>
    n >= slices
      ? Evaluator.drain_streaming_outbox(evaluation).completed
      : (
        switch (Evaluator.run_yielding_slice(~step_budget, evaluation)) {
        | EvaluationCompleted(_) =>
          Evaluator.drain_streaming_outbox(evaluation).completed
        | EvaluationYielded(evaluation) => drive(n + 1, evaluation)
        }
      );
  drive(
    0,
    Evaluator.start_yielding_evaluation(
      ~prev,
      ~eval_info,
      ~env=Builtins.env_init,
      expr,
    ),
  );
};

/* The id of the first manual refractor (probe) in the document. */
let first_probe_id = (model: CodeWithStatics.Model.t): option(Id.t) =>
  switch (model.editor.state.zipper.refractors.manuals) {
  | [(id, _), ..._] => Some(id)
  | [] => None
  };

/* The `info` a probe's renderer actually receives, built by the same
   RefractorView pass the editor uses — its `syntax` is the probed term
   unparenthesized, trimmed, and re-parenthesized, which is NOT the raw
   TermData segment. A renderer's commit reads `info.syntax`, so a test that
   passes the raw segment is testing a different input. */
let probe_info =
    (model: CodeWithStatics.Model.t, id: Id.t): ProjectorBase.info => {
  let refractors =
    Id.Map.union(
      (_, _, b) => Some(b),
      model.editor.state.zipper.refractors.manuals |> Id.Map.of_list,
      model.editor.state.zipper.refractors.multis.ephemerals,
    );
  let data =
    RefractorView.mk_data(
      ~refractors,
      ~syntax=model.editor.syntax,
      ~indicated=Indicated.for_decoration(model.editor.state.zipper),
      ~statics=model.statics.info_map,
      ~dynamics=model.dynamics,
      ~sample_focus=model.editor.state.zipper.refractors.sample_focus,
      ~editor_active=true,
    );
  switch (
    List.find_opt(
      (d: ProjectorView.Model.projector_data) => d.p.id == id,
      data,
    )
  ) {
  | Some(d) => d.info
  | None => failwith("EditorCycle.probe_info: no refractor data for that id")
  };
};

/* The index Action.SetSyntax wants: position in the refractor list. */
let refractor_idx = (model: CodeWithStatics.Model.t, id: Id.t): int =>
  switch (
    List.find_index(
      x => x == id,
      List.map(fst, model.editor.state.zipper.refractors.manuals),
    )
  ) {
  | Some(i) => i
  | None => failwith("EditorCycle.refractor_idx: not in the refractor list")
  };

/* Static errors the editor would decorate. */
let error_ids = (model: CodeWithStatics.Model.t): list(Id.t) =>
  model.statics.error_ids;
