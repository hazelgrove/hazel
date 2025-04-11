open Transition;

// Checks if a term has a cast error in redex position.
// Such a term will be an indet value.
module CastErrCheckerEVMode: {
  include EV_MODE with type result = bool and type state = unit;
} = {
  type state = unit;
  type result = bool;

  type requirement('a) = (result, 'a);
  type requirements('a, 'b) = (result, 'a, 'b);

  let req_final:
    (DHExp.t => result, EvalCtx.t => EvalCtx.t, DHExp.t) =>
    requirement(DHExp.t) =
    (h, _, d) => {
      (h(d), d);
    };
  let req_all_final:
    (
      DHExp.t => result,
      (EvalCtx.t, (list(DHExp.t), list(DHExp.t))) => EvalCtx.t,
      list(DHExp.t)
    ) =>
    requirement(list(DHExp.t)) =
    (h, _, ds) => (ds |> List.map(h) |> List.exists(x => x == true), ds);

  let otherwise: (ClosureEnvironment.t, 'a) => requirements(unit, 'a) =
    (_, r) => (false, (), r);
  let update_probe = (_, _) => ();

  let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
    ((h, a, d), rl) => {
      switch (rl(a), Exp.term_of(d)) {
      | (Indet, FailedCast(_, _, _)) => true
      | (Step(_), _) =>
        failwith("Step possible before cast failure checking") // Assume full reduction before instantiation
      // Pattern match on casts to retrieve the type to instantiate the hole
      | (Constructor, _)
      | (Value, _)
      | (Indet, _) => h
      };
    };

  let (and.):
    (requirements('a, 'c => 'b), requirement('c)) =>
    requirements(('a, 'c), 'b) =
    ((h1, a, cb), (h2, c)) => (h1 || h2, (a, c), cb(c));

  let update_test = (_, _, _) => ();
};

module CastErrChecker = Transition(CastErrCheckerEVMode);

let rec contains_error = (~in_closure=?, state, env, d) =>
  CastErrChecker.transition(
    contains_error,
    ~in_closure?,
    ~mode=`Environment,
    state,
    env,
    d,
  );

let contains_error = contains_error((), ClosureEnvironment.empty);
