open Transition;

// Finds Holes in redex position and tags with the immediate type information derived from surrounding cast.
// Match scrutinee terms which are not within casts can have multiple valid types.
type inst_cls =
  | None
  | Hole(Id.t)
  | HoleCast(Id.t, TypSlice.t)
  | Match(DHExp.t, list(DHPat.t)); // Note: scrutinee may not be a hole directly

// Locates the cast term to instantiate and extracts the hole's rep_id and cast's return type slice
module InstantiatorEVMode: {
  include EV_MODE with type result = inst_cls and type state = unit;
} = {
  type state = unit;
  type result = inst_cls;

  type requirement('a) = (result, 'a);
  type requirements('a, 'b) = (result, 'a, 'b);

  // Precedence combining holes -> matches -> casts
  // Important that holes on the left side takes precedence (so first hole term found is propagated)
  let combine = (h1: result, h2: result): result =>
    switch (h1, h2) {
    | (None, _) => h2
    | (Hole(_), None)
    | (Hole(_), Hole(_)) => h1
    | (Hole(_), _) => h2
    | (Match(_), None)
    | (Match(_), Hole(_))
    | (Match(_), Match(_)) => h1
    | (Match(_), HoleCast(_)) => h2
    | (HoleCast(_), _) => h1
    };

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
    (h, _, ds) => (ds |> List.map(h) |> List.fold_left(combine, None), ds);

  let otherwise: (ClosureEnvironment.t, 'a) => requirements(unit, 'a) =
    (_, r) => (None, (), r);

  let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
    ((h, a, d), rl) => {
      switch (rl(a), Exp.term_of(d)) {
      | (Step(_), _) => failwith("Step possible before hole instantiation") // Assume full reduction before instantiation
      // Pattern match on casts to retrieve the type to instantiate the hole
      | (Constructor, Cast(d', t1, t2))
          when Hole(d' |> DHExp.rep_id) == h && TypSlice.is_unknown(t1) =>
        HoleCast(d' |> DHExp.rep_id, t2) // Note: t1 should always be unknown, but checking to be safe
      | (Constructor, _)
      | (Value, _) => h
      | (Indet, Match(d', branches)) =>
        combine(h, Match(d', branches |> List.map(fst)))
      | (Indet, _) => combine(h, Hole(d |> DHExp.rep_id))
      };
    };

  let (and.):
    (requirements('a, 'c => 'b), requirement('c)) =>
    requirements(('a, 'c), 'b) =
    ((h1, a, cb), (h2, c)) => (combine(h1, h2), (a, c), cb(c));

  let update_test = (_, _, _) => ();
  let update_probe = (_, _) => ();
};

module Instantiator = Transition(InstantiatorEVMode);

let rec find = (~in_closure=?, state, env, d) =>
  Instantiator.transition(
    find,
    ~in_closure?,
    ~mode=`Environment,
    state,
    env,
    d,
  );

let find = (env, d) => {
  find((), ClosureEnvironment.of_environment(env), d);
};
