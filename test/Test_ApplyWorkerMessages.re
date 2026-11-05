/* EvalResult's ApplyWorkerMessages is UpdateStreamingEval (a held reuse
   plan), MergeStreamingEval (each stream) and UpdateResult (if the result
   has come), applied in that order, as one action: the page recomputes its
   view after every action, and as separate actions they cost a redraw
   each. It must leave the model exactly as they would. */
open Alcotest;
open Language;

let entry = (seq: int): IncrEval.entry(EvaluatorState.t) => {
  prev_elab: Exp.fresh(EmptyHole),
  prev_reuse_map: IncrEval.empty_reuse_map,
  prev_probe_targets: EvalInfo.ProbeTargets(SubexpProbeTargets.empty),
  value: Exp.fresh(EmptyHole),
  state: EvaluatorState.empty,
  seq,
};

let outbox = (entries: list(int)): IncrEval.outbox(EvaluatorState.t) =>
  IncrEval.outbox_of_completed({
    entries:
      List.fold_left(
        (m, seq) => Id.Map.add(Id.mk(), entry(seq), m),
        Id.Map.empty,
        entries,
      ),
  });

let result: ProgramResult.t(ProgramResult.inner) =
  ResultOk({
    result: Exp.fresh(Atom(Int(Bigint.of_int(7)))),
    state: EvaluatorState.empty,
  });

let apply = (actions, model) =>
  List.fold_left(
    (model, action) =>
      Web.EvalResult.Update.update(
        ~settings=Web.Settings.Model.init,
        action,
        model,
      ).
        model,
    model,
    actions,
  );

/* the fields these actions write */
let same = (a: Web.EvalResult.Model.t, b: Web.EvalResult.Model.t) =>
  a.result == b.result
  && a.streaming_outbox == b.streaming_outbox
  && a.streaming_state == b.streaming_state
  && a.pending_eval_ids == b.pending_eval_ids;

let case = (name, plan, streams, result) =>
  test_case(
    name,
    `Quick,
    () => {
      let init = Web.EvalResult.Model.init;
      let separate =
        apply(
          (
            switch (plan) {
            | Some(p) => [Web.EvalResult.Update.UpdateStreamingEval(p)]
            | None => []
            }
          )
          @ List.map(
              s => Web.EvalResult.Update.MergeStreamingEval(s),
              streams,
            )
          @ (
            switch (result) {
            | Some(r) => [Web.EvalResult.Update.UpdateResult(r)]
            | None => []
            }
          ),
          init,
        );
      let combined =
        apply(
          [Web.EvalResult.Update.ApplyWorkerMessages(plan, streams, result)],
          init,
        );
      check(bool, "same model", true, same(separate, combined));
    },
  );

let tests = (
  "ApplyWorkerMessages",
  [
    case(
      "plan, two streams, result",
      Some(outbox([1, 2])),
      [outbox([3]), outbox([4, 5])],
      Some(result),
    ),
    case("no plan, one stream, result", None, [outbox([1])], Some(result)),
    case(
      "plan and a stream, no result yet",
      Some(outbox([1])),
      [outbox([2])],
      None,
    ),
    case("plan, no streams, result", Some(outbox([1])), [], Some(result)),
  ],
);
