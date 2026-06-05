open Transition;

let stream_union =
    (
      left: IncrEval.t(EvaluatorState.t),
      right: IncrEval.t(EvaluatorState.t),
    )
    : IncrEval.t(EvaluatorState.t) =>
  IncrEval.add_stream(left, right);

let reusable_entry =
    (
      ~prev: EvaluatorState.incr_eval,
      ~info_map: EvalInfo.t,
      ~reuse_map: IncrEval.reuse_map,
      d: DHExp.t,
    )
    : option(IncrEval.entry(EvaluatorState.t)) =>
  IncrEval.reuse_check(
    ~call_stack=CallStack.empty,
    ~prev,
    ~reuse_map,
    ~info_map,
    ~id=DHExp.rep_id(d),
  );

module ReusePassEVMode: {
  include
    EV_MODE with
      type inner_result = (IncrEval.t(EvaluatorState.t), rule) and
      type result = (IncrEval.t(EvaluatorState.t), rule);
} = {
  type result = (IncrEval.t(EvaluatorState.t), rule);
  type inner_result = result;
  type requirement('a) = (IncrEval.t(EvaluatorState.t), 'a);
  type requirements('a, 'b) = (IncrEval.t(EvaluatorState.t), 'a, 'b);

  let req_final = (f, _, x) => {
    let (stream, _) = f(x);
    (stream, x);
  };

  let rec req_all_final = (f, i, xs) =>
    switch (xs) {
    | [] => (IncrEval.empty, [])
    | [x, ...xs] =>
      let (stream, x) = req_final(f, x => x, x);
      let (streams, xs) = req_all_final(f, i, xs);
      (stream_union(stream, streams), [x, ...xs]);
    };

  let otherwise = (_, c) => (IncrEval.empty, (), c);

  let (and.) = ((stream1, x1, c1), (stream2, x2)) => (
    stream_union(stream1, stream2),
    (x1, x2),
    c1(x2),
  );

  let (let.) = ((stream, x, _), s) => (stream, s(x));
};

module ReusePassTransition = Transition(ReusePassEVMode);

let update_reuse_map_after_effects =
    (
      ~rhs_reused: Id.t => bool,
      ~reuse_map: IncrEval.reuse_map,
      effects: list(EvaluatorState.effect),
    )
    : IncrEval.reuse_map =>
  List.fold_left(
    (reuse_map, effect) =>
      switch (effect) {
      | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
        let source_id = DHExp.rep_id(rhs);
        IncrEval.update_maps_after_binding(
          ~rhs_reused=rhs_reused(source_id),
          ~source_id,
          pat,
          ~reuse_map,
        );
      | _ => reuse_map
      },
    reuse_map,
    effects,
  );

let rec reuse_pass_for =
        (
          ~prev: EvaluatorState.incr_eval,
          ~info_map: EvalInfo.t,
          ~reuse_map: IncrEval.reuse_map,
          d: DHExp.t,
        )
        : IncrEval.t(EvaluatorState.t) => {
  let id = DHExp.rep_id(d);
  switch (reusable_entry(~prev, ~info_map, ~reuse_map, d)) {
  | Some(entry) => {entries: Id.Map.add(id, entry, Id.Map.empty)}
  | None =>
    let (req_stream, rule) =
      ReusePassTransition.transition(
        (~in_closure=?, _env, child) => {
          ignore(in_closure);
          (reuse_pass_for(~prev, ~info_map, ~reuse_map, child), Indet);
        },
        ~mode=`Environment,
        ~targets=info_map.targets,
        Builtins.env_init,
        d,
      );
    switch (rule) {
    | Step({expr, side_effects, is_value: false, _}) =>
      let reuse_map =
        update_reuse_map_after_effects(
          ~rhs_reused=source_id => Id.Map.mem(source_id, req_stream.entries),
          ~reuse_map,
          side_effects,
        );
      stream_union(
        req_stream,
        reuse_pass_for(~prev, ~info_map, ~reuse_map, expr),
      );
    | Step({is_value: true, _})
    | Constructor
    | Value
    | Indet => req_stream
    };
  };
};

let reuse_pass =
    (
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~info_map: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: IncrEval.reuse_map=IncrEval.clean_reuse_map_of_env(env),
      d: DHExp.t,
    )
    : IncrEval.t(EvaluatorState.t) =>
  reuse_pass_for(~prev, ~info_map, ~reuse_map, d);
