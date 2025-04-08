open Transition;

module TryStep = {
  type t =
    | Indet
    | BoxedValue
    | Step(DHExp.t);

  let unbox = (r: t): option(DHExp.t) => {
    switch (r) {
    | Indet
    | BoxedValue => None
    | Step(e) => Some(e)
    };
  };
};

module OneStepEVMode: {
  include
    EV_MODE with
      type result = TryStep.t and type state = ref(EvaluatorState.t);
} = {
  type state = ref(EvaluatorState.t);
  type requirement('a) = (TryStep.t, 'a);
  type requirements('a, 'b) = (TryStep.t, 'a);
  type result = TryStep.t;

  let (&&): (TryStep.t, TryStep.t) => TryStep.t =
    (u, v) =>
      switch (u, v) {
      | (Step(e), _) // First step takes precedence
      | (_, Step(e)) => Step(e)
      | (Indet, BoxedValue)
      | (BoxedValue, Indet)
      | (Indet, Indet) => Indet
      | (BoxedValue, BoxedValue) => BoxedValue
      };

  let req_final = (cont, _, d) => {
    (cont(d), d);
  };

  let rec req_all_final = (cont, _, ds) =>
    List.fold_right(
      ((r, v), (r_acc, v_acc)) => (r && r_acc, [v, ...v_acc]),
      List.map(req_final(cont, x => x), ds),
      (BoxedValue, []),
    );

  let (let.) = (rq, rl) =>
    switch (rq) {
    | (TryStep.Indet, _) => TryStep.Indet
    | (TryStep.BoxedValue, v) =>
      switch (rl(v)) {
      | Constructor => TryStep.BoxedValue
      | Value => TryStep.BoxedValue
      | Indet => TryStep.Indet
      | Step(s) => TryStep.Step(s.expr)
      }
    | (TryStep.Step(_) as r, _) => r
    };

  let (and.):
    (requirements('a, 'c => 'b), requirement('c)) =>
    requirements(('a, 'c), 'b) =
    ((r1, v1), (r2, v2)) => (r1 && r2, (v1, v2));

  let otherwise = (_, _) => (TryStep.BoxedValue, ());
  let atom = otherwise;
  let update_test = (state, id, v) =>
    state := EvaluatorState.add_test(state^, id, v);
  let update_probe = (state, closure: Dynamics.Probe.Closure.t) =>
    state := EvaluatorState.add_closure(state^, closure);
};

module OneStep = Transition(OneStepEVMode);
let rec step = (~in_closure=?, state, env, exp) => {
  switch (exp) {
  | _ => OneStep.transition(step, ~in_closure?, state, env, exp)
  };
};
