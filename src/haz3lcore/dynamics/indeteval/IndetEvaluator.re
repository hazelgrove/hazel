open ProgramResult.Result;
open Nondeterminism;
open Transition;
open Util.Sequence;

// TODO: for the stepper add logging: i.e. return a sequence of exps paired with a step-kind detailing the instantiation made. Slice info is available for this
module Make = (S: Search) => {
  module Searching = Searching.Make(S);
  module Instantiation = Instantiation.Make(S);
  open S;
  open S.Infix;
  open S.Syntax;

  // Framework behind indeterminately evaluates, with custom logic for deterining what constitutes as a search result.
  // - Boxed values are checked if they are results
  // - Indet terms are checked as results then are instantiated nondeterministically
  // - Expressions are checked as results then further evaluated
  let rec search = (logic: Searching.t('a), env, d: DHExp.t): t('a) => {
    // TODO: State threading?
    let state = ref(EvaluatorState.init);
    let env' = ClosureEnvironment.of_environment(env);
    let step = OneStepEvaluator.step(state, env', d);
    switch (step) {
    | BoxedValue => logic(d, Searching.BoxedValue)
    | Indet =>
      logic(d, Indet)
      <|> wrap(Instantiation.instantiate(env, d) >>- search(logic, env))
    | Step(d') => logic(d, Expr) <|> wrap(search(logic, env, d'))
    | exception (EvaluatorError.Exception(_)) => fail
    };
  };

  // Same as search, but allows full customisation of state space expansion
  // (note: does NOT automatically instantiate indets, or continue evaluation)
  let rec search_expert =
          (logic: Searching.expert_t('a), env, d: DHExp.t): t('a) => {
    // TODO: State threading?
    let state = ref(EvaluatorState.init);
    let env_c = ClosureEnvironment.of_environment(env);
    let step = OneStepEvaluator.step(state, env_c, d);
    switch (step) {
    | exception (EvaluatorError.Exception(_)) => fail
    | step =>
      let (results, space) = logic(d, step);
      results <|> wrap(space >>- search_expert(logic, env));
    };
  };

  // Standard expressions to search for
  let boxed_values = search(Searching.boxed_values);
  let indet_values = search(Searching.indet_values);
  let expressions = search(Searching.expressions);
  let values = search(Searching.values);

  let cast_errors = search(Searching.cast_error);
  let deterministic: (Environment.t, DHExp.t) => S.t(DHExp.t) =
    search_expert(Searching.deterministic);
  let deterministic_cast_errors =
    search_expert(Searching.no_instantiation(Searching.cast_error));
};
