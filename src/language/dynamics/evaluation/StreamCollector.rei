let collect_stream_state:
  (IncrEval.outbox(EvaluatorState.t), DHExp.t) => EvaluatorState.t;

/* Incremental collector state (opaque): thread it between chunks;
   keyed inside by elab identity. */
module Inc: {
  type t;
};

/* As [collect_stream_state], but O(chunk + frontier) per call instead
   of an O(program) walk. Returns the state to thread into the next
   call; None means it fell back to the full walk. */
let collect_stream_state_inc:
  (~prev: option(Inc.t), IncrEval.outbox(EvaluatorState.t), DHExp.t) =>
  (option(Inc.t), EvaluatorState.t);
