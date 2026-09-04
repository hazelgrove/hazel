open Transition;

module CollectStreamEVMode =
  AccumulatingEVMode.Make({
    type t = EvaluatorState.t;
    let empty = EvaluatorState.empty;
    let combine = EvaluatorState.append;
  });

module CollectStreamTransition = Transition(CollectStreamEVMode);

let rec collect_stream_state_for =
        (stream: IncrEval.outbox(EvaluatorState.t), d: DHExp.t)
        : EvaluatorState.t => {
  let id = DHExp.rep_id(d);
  switch (Id.Map.find_opt(id, stream.completed.entries)) {
  | Some(entry) =>
    let state = EvaluatorState.rebase(entry.state);
    let state = EvaluatorState.add_incr_entry(state, id, entry);
    state;
  | None =>
    switch (stream.current) {
    /* Id.invalid is shared by all Exp.temp nodes (probes off). Matching it
     * here collides with temps this walk itself creates and truncates
     * collection — streamed results appear to go backwards. */
    | Some({id: current_id, state})
        when Id.equal(id, current_id) && !Id.equal(current_id, Id.invalid) =>
      EvaluatorState.rebase(state)
    | Some(_)
    | None =>
      let (req_state, rule) =
        CollectStreamTransition.transition(
          (~in_closure=?, _env, child) => {
            ignore(in_closure);
            (collect_stream_state_for(stream, child), Indet);
          },
          ~mode=`Environment,
          ~targets=Sample.no_targets,
          Builtins.env_init,
          d,
        );
      switch (rule) {
      | Step({expr, is_value: false, _}) =>
        EvaluatorState.append(
          req_state,
          collect_stream_state_for(stream, expr),
        )
      | Step({is_value: true, _})
      | Constructor
      | Value
      | Indet => req_state
      };
    }
  };
};

let collect_stream_state =
    (stream: IncrEval.outbox(EvaluatorState.t), d: DHExp.t): EvaluatorState.t => {
  let state = collect_stream_state_for(stream, d);
  {
    ...state,
    incr_eval: {
      entries:
        Id.Map.union(
          (_, existing, _streamed) => Some(existing),
          state.incr_eval.entries,
          stream.completed.entries,
        ),
    },
  };
};
