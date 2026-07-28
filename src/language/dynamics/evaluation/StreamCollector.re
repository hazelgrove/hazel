open Transition;

module CollectStreamEVMode: {
  include
    EV_MODE with
      type inner_result = (EvaluatorState.t, rule) and
      type result = (EvaluatorState.t, rule);
} = {
  type result = (EvaluatorState.t, rule);
  type inner_result = result;
  type requirement('a) = (EvaluatorState.t, 'a);
  type requirements('a, 'b) = (EvaluatorState.t, 'a, 'b);

  let req_final = (f, _, x) => {
    let (state, _) = f(x);
    (state, x);
  };

  let rec req_all_final = (f, i, xs) =>
    switch (xs) {
    | [] => (EvaluatorState.empty, [])
    | [x, ...xs] =>
      let (state, x) = req_final(f, x => x, x);
      let (states, xs) = req_all_final(f, i, xs);
      (EvaluatorState.append(state, states), [x, ...xs]);
    };

  let otherwise = (_, c) => (EvaluatorState.empty, (), c);

  let (and.) = ((state1, x1, c1), (state2, x2)) => (
    EvaluatorState.append(state1, state2),
    (x1, x2),
    c1(x2),
  );

  let (let.) = ((state, x, _), s) => (state, s(x));
};

module CollectStreamTransition = Transition(CollectStreamEVMode);

let rec collect_stream_state_for =
        (stream: IncrEval.outbox(EvaluatorState.t), d: DHExp.t)
        : EvaluatorState.t => {
  let id = DHExp.rep_id(d);
  switch (Id.Map.find_opt(id, stream.completed.entries)) {
  | Some(entry) =>
    let state = EvaluatorState.append(EvaluatorState.empty, entry.state);
    let state = EvaluatorState.add_incr_entry(state, id, entry);
    state;
  | None =>
    switch (stream.current) {
    /* Id.invalid is shared by all Exp.temp nodes (probes off). Matching it
     * here collides with temps this walk itself creates and truncates
     * collection — streamed results appear to go backwards. */
    | Some({id: current_id, state})
        when Id.equal(id, current_id) && !Id.equal(current_id, Id.invalid) =>
      EvaluatorState.append(EvaluatorState.empty, state)
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
