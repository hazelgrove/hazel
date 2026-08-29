/* WorkerServer stream-interest filtering: with Effects interest, only
   entries carrying tests/probes/theorems ship; husks (ids + step
   counts, consumed only by the pending-eval highlight) are dropped and
   a filtered-to-empty chunk is not posted (outbox_is_empty). */

open Alcotest;
open Language;

let mk_entry = (state: EvaluatorState.t): IncrEval.entry(EvaluatorState.t) => {
  prev_elab: Exp.fresh(EmptyHole),
  prev_reuse_map: IncrEval.empty_reuse_map,
  prev_probe_targets: EvalInfo.ProbeTargets(SubexpProbeTargets.empty),
  value: Exp.fresh(EmptyHole),
  state,
  seq: 0,
};

let husk = mk_entry(EvaluatorState.empty_at(0));
let with_test =
  mk_entry({
    ...EvaluatorState.empty_at(0),
    tests: [(Id.mk(), [])],
  });

let outbox_of = (entries: list((Id.t, IncrEval.entry(EvaluatorState.t)))) =>
  IncrEval.{
    completed: {
      entries: entries |> List.to_seq |> Id.Map.of_seq,
    },
    current: None,
  };

let count = (u: IncrEval.outbox(EvaluatorState.t)) =>
  Id.Map.cardinal(u.completed.entries);

let tests = [
  (
    "StreamInterests",
    [
      test_case("husk entries carry no effects", `Quick, () =>
        check(bool, "husk", false, WorkerServer.entry_has_effects(husk))
      ),
      test_case("test-bearing entries carry effects", `Quick, () =>
        check(
          bool,
          "test",
          true,
          WorkerServer.entry_has_effects(with_test),
        )
      ),
      test_case("Effects interest drops husks, keeps tests", `Quick, () => {
        WorkerServer.current_stream_interest := WorkerServer.Request.Effects;
        let u =
          outbox_of([(Id.mk(), husk), (Id.mk(), with_test)])
          |> WorkerServer.filter_stream_interest;
        check(int, "kept", 1, count(u));
      }),
      test_case("Effects interest: all-husk chunk becomes empty", `Quick, () => {
        WorkerServer.current_stream_interest := WorkerServer.Request.Effects;
        let u =
          outbox_of([(Id.mk(), husk)])
          |> WorkerServer.filter_stream_interest;
        check(bool, "empty", true, IncrEval.outbox_is_empty(u));
      }),
      test_case("Full interest ships everything", `Quick, () => {
        WorkerServer.current_stream_interest := WorkerServer.Request.Full;
        let u =
          outbox_of([(Id.mk(), husk), (Id.mk(), with_test)])
          |> WorkerServer.filter_stream_interest;
        check(int, "kept", 2, count(u));
      }),
    ],
  ),
];
