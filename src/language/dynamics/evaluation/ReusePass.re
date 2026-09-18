open Transition;

let reusable_entry =
    (
      ~prev: EvaluatorState.incr_eval,
      ~eval_info: EvalInfo.t,
      ~reuse_map: IncrEval.reuse_map,
      d: DHExp.t,
    )
    : option(IncrEval.entry(EvaluatorState.t)) =>
  IncrEval.reuse_check(
    /* The pre-pass predicts top-level re-use only: it walks the program
     * without a callstack, so a2's nested entries are out of its reach. */
    ~call_stack_ids=Some([]),
    ~prev,
    ~reuse_map,
    ~eval_info,
    ~id=DHExp.rep_id(d),
  );

module ReusePassEVMode =
  AccumulatingEVMode.Make({
    type t = IncrEval.t(EvaluatorState.t);
    let empty = IncrEval.empty;
    let combine = IncrEval.add_stream;
  });

module ReusePassTransition = Transition(ReusePassEVMode);

/* `flags_from` is the map a binding's flag is read off, which is normally the
 * map being extended. They come apart at a2's call boundary: the argument was
 * evaluated in the caller's environment, so its flag belongs to the caller's
 * map, while the binding lands in the body's. */
let update_reuse_map_after_effects =
    (
      ~tuple_flags: bool,
      ~reused: Id.t => bool,
      ~flags_from: option(IncrEval.reuse_map)=?,
      ~reuse_map: IncrEval.reuse_map,
      effects: list(EvaluatorState.effect),
    )
    : IncrEval.reuse_map =>
  List.fold_left(
    (reuse_map, effect) =>
      switch (effect) {
      | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
        /* rhs is the binding's right-hand side before evaluation, so its flag
         * is read off the re-use map rather than off a value. */
        IncrEval.update_maps_after_binding(
          ~flag=
            IncrEval.exp_flag(
              ~tuple_flags,
              ~reused,
              ~reuse_map=
                switch (flags_from) {
                | Some(flags_from) => flags_from
                | None => reuse_map
                },
              rhs,
            ),
          ~source_id=DHExp.rep_id(rhs),
          pat,
          ~reuse_map,
        )
      | _ => reuse_map
      },
    reuse_map,
    effects,
  );

let rec reuse_pass_for =
        (
          ~tuple_flags: bool,
          ~prev: EvaluatorState.incr_eval,
          ~eval_info: EvalInfo.t,
          ~reuse_map: IncrEval.reuse_map,
          d: DHExp.t,
        )
        : IncrEval.t(EvaluatorState.t) => {
  let id = DHExp.rep_id(d);
  switch (reusable_entry(~prev, ~eval_info, ~reuse_map, d)) {
  | Some(entry) => IncrEval.add_entry(id, entry, IncrEval.empty)
  | None =>
    let (req_stream, rule) =
      ReusePassTransition.transition(
        (~in_closure=?, _env, child) => {
          ignore(in_closure);
          (
            reuse_pass_for(~tuple_flags, ~prev, ~eval_info, ~reuse_map, child),
            Indet,
          );
        },
        ~mode=`Environment,
        ~targets=eval_info.targets,
        Builtins.env_init,
        d,
      );
    switch (rule) {
    | Step({expr, side_effects, is_value: false, _}) =>
      let reuse_map =
        update_reuse_map_after_effects(
          ~tuple_flags,
          ~reused=id => Id.Map.mem(id, req_stream.entries),
          ~reuse_map,
          side_effects,
        );
      IncrEval.add_stream(
        req_stream,
        reuse_pass_for(~tuple_flags, ~prev, ~eval_info, ~reuse_map, expr),
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
      ~tuple_flags: bool=false,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~eval_info: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: IncrEval.reuse_map=IncrEval.clean_reuse_map_of_env(env),
      d: DHExp.t,
    )
    : IncrEval.t(EvaluatorState.t) =>
  reuse_pass_for(~tuple_flags, ~prev, ~eval_info, ~reuse_map, d);
