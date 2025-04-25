open Transition;

module TryStep = {
  type t('a) =
    | Indet
    | BoxedValue
    | Step('a);

  let unbox = (r: t('a)): option('a) => {
    switch (r) {
    | Indet
    | BoxedValue => None
    | Step(e) => Some(e)
    };
  };

  let bind = f =>
    fun
    | Indet => Indet
    | BoxedValue => BoxedValue
    | Step(s) => f(s);

  let map = f =>
    fun
    | Indet => Indet
    | BoxedValue => BoxedValue
    | Step(s) => Step(f(s));

  module Syntax = {
    let ( let* ) = (s, f) => bind(f, s);
    let (let+) = (s, f) => map(f, s);
  };
};

module WrapStep = {
  module WrapEVMode: {
    include
      EV_MODE with
        type result = TryStep.t(EvalObj.t) and
        type state = ref(IndetEvaluatorState.t);
  } = {
    type state = ref(IndetEvaluatorState.t);
    type requirement('a) = (TryStep.t(EvalObj.t), 'a);
    type requirements('a, 'b) = (
      'b,
      TryStep.t(EvalObj.t),
      ClosureEnvironment.t,
      'a,
    );
    type result = TryStep.t(EvalObj.t);

    let (&&):
      (TryStep.t(EvalObj.t), TryStep.t(EvalObj.t)) => TryStep.t(EvalObj.t) =
      (u, v) =>
        switch (u, v) {
        | (Step(s), Step(_)) // Get only first step
        | (Step(s), _)
        | (_, Step(s)) => Step(s)
        | (Indet, BoxedValue)
        | (BoxedValue, Indet)
        | (Indet, Indet) => Indet
        | (BoxedValue, BoxedValue) => BoxedValue
        };

    let req_final = (cont, wr, d) => {
      (
        switch (cont(d)) {
        | TryStep.Indet => TryStep.Indet
        | BoxedValue => BoxedValue
        | Step(obj) => Step(EvalObj.wrap(wr, obj))
        },
        d,
      );
    };

    let rec req_all_final' = (cont, wr, ds') =>
      fun
      | [] => (TryStep.BoxedValue, [])
      | [d, ...ds] => {
          let (r1, v) = req_final(cont, wr(_, (ds', ds)), d);
          let (r2, vs) = req_all_final'(cont, wr, [d, ...ds'], ds);
          (r1 && r2, [v, ...vs]);
        };

    let req_all_final = (cont, wr, ds) => {
      req_all_final'(cont, wr, [], ds);
    };

    let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
      (rq, rl) => {
        switch (rq) {
        | (_, Step(_) as s, _, _) => s
        | (undo, r, env, v) =>
          switch (rl(v)) {
          | Constructor => r
          | Value => BoxedValue
          | Indet => Indet
          | Step(s) => Step(EvalObj.mk(Mark, env, undo, s.kind))
          }
        };
      };
    let (and.):
      (requirements('a, 'c => 'b), requirement('c)) =>
      requirements(('a, 'c), 'b) =
      ((u, r1, env, v1), (r2, v2)) => (u(v2), r1 && r2, env, (v1, v2));

    let otherwise = (env, o) => (o, TryStep.BoxedValue, env, ());
    let update_test = (state, id, v) =>
      state := IndetEvaluatorState.add_test(state^, id, v);
    let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
      state := IndetEvaluatorState.add_closure(state^, closure);
  };

  module Wrap = Transition(WrapEVMode);
  let rec wrap = (~in_closure=?, state, env, exp) => {
    Wrap.transition(wrap, ~in_closure?, ~mode=`Environment, state, env, exp);
  };

  let wrap = (state, env, d) => {
    let state = ref(state);
    let env = ClosureEnvironment.of_environment(env);
    let result = wrap(state, env, d);
    (result, state^); // Thread state throughout
  };
};

module TakeStep = {
  module TakeStepEVMode: {
    include
      EV_MODE with
        type result = option(DHExp.t) and
        type state = ref(IndetEvaluatorState.t);
  } = {
    type state = ref(IndetEvaluatorState.t);
    type requirement('a) = 'a;
    type requirements('a, 'b) = 'a;
    type result = option(DHExp.t);

    // Assume that everything is either value or final as required.
    let req_final = (_, _, d) => d;
    let req_all_final = (_, _, ds) => ds;

    let (let.) = (rq: requirements('a, DHExp.t), rl: 'a => rule) =>
      switch (rl(rq)) {
      | Step({expr, state_update, _}) =>
        state_update();
        Some(expr);
      | Constructor
      | Value
      | Indet => None
      };

    let (and.) = (x1, x2) => (x1, x2);

    let otherwise = (_, _) => ();
    let atom = otherwise;

    let update_test = (state, id, v) =>
      state := IndetEvaluatorState.add_test(state^, id, v);

    let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
      state := IndetEvaluatorState.add_closure(state^, closure);
  };

  module TakeStepEV = Transition(TakeStepEVMode);

  let step = (~in_closure=?, state, env, d) =>
    TakeStepEV.transition(
      (~in_closure as _=?, _, _, _) => None,
      ~in_closure?,
      ~mode=`Environment,
      state,
      env,
      d,
    )
    |> Option.map(DHExp.replace_all_ids);
};

let take_step = (state, env, exp) => {
  open TryStep.Syntax;
  let (step, next_state) = WrapStep.wrap(state, env, exp);
  let next_state = ref(next_state);
  let next_step = {
    let* step = step;
    switch (TakeStep.step(next_state, step.env, step.d_loc)) {
    | None => Indet
    | Some(next_expr) =>
      let next_step =
        {
          term: next_expr.term,
          annotation: IdTagged.IdTag.{ids: step.d_loc |> IdTagged.ids},
        }
        |> EvalCtx.compose(step.ctx)
        |> Exp.replace_all_ids;
      //|> DHExp.substitute_closures(env);
      Step(next_step);
    };
  };
  (next_step, next_state^);
};
