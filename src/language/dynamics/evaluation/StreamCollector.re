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

/* incremental collector: the merged state depends only on the outermost
   completed regions (an entry's slice subsumes its subtree's entries)
   plus the in-flight partial, so index the elab once (DFS enter/exit)
   and fold only the frontier of outermost entries per chunk. in-flight
   goes last: mid-run it follows every completed region, as outer spine
   entries complete only at the end */
module Inc = {
  type t = {
    inc_elab: DHExp.t, /* identity key: new elab = new evaluation */
    enter: Id.Map.t(int),
    exit_: Id.Map.t(int),
    processed: Id.Map.t(unit),
    /* (pos_seq, enter, exit, entry) of outermost completed entries,
       ordered by record-time step count (the full walk's append order);
       intervals only serve coverage tests, and a covering entry takes
       the first subsumed entry's position */
    frontier: list((int, int, int, IncrEval.entry(EvaluatorState.t))),
  };

  let index = (d: DHExp.t): (Id.Map.t(int), Id.Map.t(int)) => {
    let enter = ref(Id.Map.empty);
    let exit_ = ref(Id.Map.empty);
    let c = ref(0);
    let f_exp = (continue, e: Exp.t): Exp.t => {
      let id = Exp.rep_id(e);
      /* first occurrence wins: temp/invalid ids repeat, and only the
         outermost can carry a relevant cache entry */
      if (!Id.Map.mem(id, enter^)) {
        enter := Id.Map.add(id, c^, enter^);
        incr(c);
        let e = continue(e);
        exit_ := Id.Map.add(id, c^, exit_^);
        incr(c);
        e;
      } else {
        let e = continue(e);
        incr(c);
        e;
      };
    };
    let _ = TermBase.Exp.map_term(~f_exp, d);
    (enter^, exit_^);
  };

  let fresh = (d: DHExp.t): t => {
    let (enter, exit_) = index(d);
    {
      inc_elab: d,
      enter,
      exit_,
      processed: Id.Map.empty,
      frontier: [],
    };
  };

  /* None when the stream names an id the index lacks; the caller then
     falls back to the full walk */
  let absorb =
      (inc: t, stream: IncrEval.outbox(EvaluatorState.t)): option(t) =>
    Id.Map.fold(
      (id, entry, acc) =>
        switch (acc) {
        | None => None
        | Some(inc) =>
          if (Id.Map.mem(id, inc.processed)) {
            Some(inc);
          } else {
            let processed = Id.Map.add(id, (), inc.processed);
            switch (Id.Map.find_opt(id, inc.enter)) {
            | None => None
            | Some(en) =>
              let ex = Id.Map.find(id, inc.exit_);
              let covered =
                List.exists(
                  ((_, fe, fx, _)) => fe <= en && ex <= fx,
                  inc.frontier,
                );
              if (covered) {
                Some({
                  ...inc,
                  processed,
                });
              } else {
                let covers = ((_, fe, fx, _)) => en <= fe && fx <= ex;
                let frontier =
                  if (List.exists(covers, inc.frontier)) {
                    /* take the first subsumed node's position (where
                       the walk appends the outer slice); drop the rest */
                    let rec go = (replaced, l) =>
                      switch (l) {
                      | [] =>
                        replaced ? [] : [(entry.IncrEval.seq, en, ex, entry)]
                      | [(ps, _, _, _) as hd, ...tl] when covers(hd) =>
                        replaced
                          ? go(replaced, tl)
                          : [(ps, en, ex, entry), ...go(true, tl)]
                      | [hd, ...tl] => [hd, ...go(replaced, tl)]
                      };
                    go(false, inc.frontier);
                  } else {
                    /* fresh disjoint region: insert by seq */
                    let sq = entry.IncrEval.seq;
                    let rec ins = l =>
                      switch (l) {
                      | [] => [(sq, en, ex, entry)]
                      | [(ps, _, _, _) as hd, ...tl] when ps <= sq => [
                          hd,
                          ...ins(tl),
                        ]
                      | l => [(sq, en, ex, entry), ...l]
                      };
                    ins(inc.frontier);
                  };
                Some({
                  ...inc,
                  processed,
                  frontier,
                });
              };
            };
          }
        },
      stream.completed.entries,
      Some(inc),
    );

  let collect =
      (inc: t, stream: IncrEval.outbox(EvaluatorState.t)): EvaluatorState.t => {
    let state =
      List.fold_left(
        (acc, (_, _, _, entry: IncrEval.entry(EvaluatorState.t))) =>
          EvaluatorState.append(acc, EvaluatorState.rebase(entry.state)),
        EvaluatorState.empty,
        inc.frontier,
      );
    let state =
      switch (stream.current) {
      | Some({id, state: cur})
          when
            !Id.equal(id, Id.invalid)
            && !
                 List.exists(
                   ((_, fe, fx, _)) =>
                     switch (Id.Map.find_opt(id, inc.enter)) {
                     | Some(en) => fe <= en && en <= fx
                     | None => false
                     },
                   inc.frontier,
                 ) =>
        EvaluatorState.append(state, EvaluatorState.rebase(cur))
      | _ => state
      };
    {
      ...state,
      incr_eval: {
        entries: stream.completed.entries,
      },
    };
  };
};

let collect_stream_state_inc =
    (
      ~prev: option(Inc.t),
      stream: IncrEval.outbox(EvaluatorState.t),
      d: DHExp.t,
    )
    : (option(Inc.t), EvaluatorState.t) => {
  let inc =
    switch (prev) {
    | Some(inc) when inc.Inc.inc_elab === d => inc
    | _ => Inc.fresh(d)
    };
  switch (Inc.absorb(inc, stream)) {
  | Some(inc) => (Some(inc), Inc.collect(inc, stream))
  | None => (None, collect_stream_state(stream, d))
  };
};
