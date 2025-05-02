open Nondeterminism;

// TODO: for the stepper add logging: i.e. return a sequence of exps paired with a step-kind detailing the instantiation made. Slice info is available for this
module Make = (S: Search) => {
  module Searching = Searching.Make(S);
  module Instantiation = Instantiation.Make(S);
  open S;
  open S.Infix;

  // Framework behind indeterminately evaluates, with custom logic for deterining what constitutes as a search result.
  // - Boxed values are checked if they are results
  // - Indet terms are checked as results then are instantiated nondeterministically
  // - Expressions are checked as results then further evaluated
  // TODO: Threading env and state correctly (currently variables will be lost as I don't use extended env to evaluate them)
  let rec search =
          (
            ~logic: Searching.t('a),
            ~state=IndetEvaluatorState.init,
            ~env,
            d: DHExp.t,
          ) => {
    let (step, next_state) = OneStepEvaluator.take_step(state, env, d);
    let search = ((state, d)) => search(~logic, ~env, ~state, d); // env cannot change even after a step
    let inject_state = (state, r) => (state, r);
    switch (step) {
    | BoxedValue =>
      logic(d, Searching.BoxedValue) >>| inject_state(next_state)
    | Indet =>
      let next_state_after_instantiation =
        IndetEvaluatorState.incr_instantiations(1, next_state);
      logic(d, Indet)
      >>| inject_state(next_state)
      <|> wrap(
            d
            |>- Instantiation.instantiate(env)
            >>| inject_state(next_state_after_instantiation)
            >>= search,
          );
    | Step(exp) =>
      let next_state_after_step =
        IndetEvaluatorState.incr_trace(1, next_state);
      logic(d, Expr)
      >>| inject_state(next_state)
      <|> wrap(exp |> inject_state(next_state_after_step) |>- search);
    | exception (EvaluatorError.Exception(_)) => fail
    };
  };

  // Same as search, but allows full customisation of state space expansion
  // (note: does NOT automatically instantiate indets, or continue evaluation)
  let rec search_expert =
          (~logic: Searching.expert_t('a, 'state), ~env, ~state, d: DHExp.t)
          : t(('state, 'a)) => {
    let (step, next_state) = OneStepEvaluator.take_step(state, env, d);
    let search = ((state, d)) => search_expert(~logic, ~env, ~state, d); // env cannot change even after a step
    switch (step) {
    | exception (EvaluatorError.Exception(_)) => fail
    | step =>
      let (results, space) = logic(next_state, d, step);
      results <|> wrap(space >>= search);
    };
  };

  // Standard expressions to search for
  let all = search(~logic=Searching.all);
  let boxed_values = search(~logic=Searching.boxed_values);
  let indet_values = search(~logic=Searching.indet_values);
  let expressions = search(~logic=Searching.expressions);
  let values = search(~logic=Searching.values);

  let cast_errors = search(~logic=Searching.cast_error);
  let deterministic = search_expert(~logic=Searching.deterministic);
  let deterministic_cast_errors =
    search_expert(~logic=Searching.no_instantiation(Searching.cast_error));
};
