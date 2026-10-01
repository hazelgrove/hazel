let collect_stream_state:
  (IncrEval.outbox(EvaluatorState.t), DHExp.t) => EvaluatorState.t;

/* collector state threaded between chunks; keyed by elab identity */
module Inc: {
  type t;
};

/* as [collect_stream_state], but O(chunk + frontier) per call; returns
   the state for the next call, None meaning it fell back to the walk */
let collect_stream_state_inc:
  (~prev: option(Inc.t), IncrEval.outbox(EvaluatorState.t), DHExp.t) =>
  (option(Inc.t), EvaluatorState.t);
