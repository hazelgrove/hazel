open Transition;

type inst_cls =
  | None
  | Hole(DHExp.t)
  | HoleCast(DHExp.t, TypSlice.t)
  | IndetPat(DHExp.t, list(DHPat.t));

// Locates the cast term or match term to instantiate and extracts the hole's rep_id and cast's return type slice
module InstantiatorEVMode: {
  include EV_MODE with type result = inst_cls and type state = unit;
} = {
  type state = unit;
  type result = inst_cls;

  type requirement('a) = (result, 'a);
  type requirements('a, 'b) = (result, 'a, 'b);

  // Precedence combining holes/casts/matches
  let combine = (h1: result, h2: result): result =>
    switch (h1, h2) {
    | (None, _) => h2
    | (Hole(_), None)
    | (Hole(_), Hole(_)) => h1
    | (Hole(_), _) => h2
    | (HoleCast(_), None)
    | (HoleCast(_), Hole(_))
    | (HoleCast(_), HoleCast(_)) => h1
    | (HoleCast(_), IndetPat(_)) => h2
    | (IndetPat(_), _) => h1
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
      | (Constructor, Cast(_, t1, t2))
          // Note: t1 should always be unknown, but checking to be safe
          when TypSlice.is_unknown(t1) =>
        switch (h) {
        | Hole(d') => HoleCast(d', t2) // Note: d' must be the term in the cast
        | _ => h
        }
      | (Constructor, _)
      | (Value, _) => h
      | (IndetMatch(rules, d'), _) => combine(h, IndetPat(d', rules))
      | (Indet, _) => combine(h, Hole(d))
      };
    };

  let (and.):
    (requirements('a, 'c => 'b), requirement('c)) =>
    requirements(('a, 'c), 'b) =
    ((h1, a, cb), (h2, c)) => (combine(h1, h2), (a, c), cb(c));

  let update_test = (_, _, _) => ();
};

module Instantiator = Transition(InstantiatorEVMode);

let rec find = (~in_closure=?, state, env, d) =>
  Instantiator.transition(find, ~in_closure?, state, env, d);

// TODO: for the stepper add logging: i.e. return a sequence of exps paired with a step-kind detailing the instantiation made. Slice info is available for this
let instantiate = (env, d) => {
  let env = ClosureEnvironment.of_environment(env);
  switch (find((), env, d)) {
  | None
  | Hole(_) => Futures.empty
  | HoleCast(hole, slc) =>
    Instantiation.(construct_typ(DHExp.rep_id(hole), slc) |> subst(d))
  | IndetPat(indet, pat) =>
    failwith("TODO: Instantiating indets against pattern")
  };
};
