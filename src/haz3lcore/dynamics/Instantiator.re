open Transition;

// Locates the cast term to instantiate and extracts the hole's rep_id and cast's return type slice
module InstantiatorEVMode: {
  include
    EV_MODE with
      type result = option((DHExp.t, option(TypSlice.t))) and
      type state = unit;
} = {
  type state = unit;
  type result = option((DHExp.t, option(TypSlice.t)));

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
    (h, _, ds) => (
      ds |> Util.ListUtil.hd_opt |> Option.map(h) |> Option.join,
      ds,
    );

  let otherwise: (ClosureEnvironment.t, 'a) => requirements(unit, 'a) =
    (_, r) => (None, (), r);

  let combine = (h1: result, h2: result): result =>
    switch (h1) {
    | Some(d) => Some(d)
    | None => h2
    };

  let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
    ((h, a, d), rl) => {
      //hs
      //|> List.fold_left((acc, d) => acc ++ "\nINDET: " ++ Exp.show(d), "")
      //|> print_endline;
      //d |> Exp.show |> (x => print_endline("TERM: " ++ x));
      switch (rl(a), Exp.term_of(d)) {
      | (Step(_), _) => failwith("Step possible before hole instantiation") // Assume full reduction before instantiation
      // Pattern match on casts to retrieve the type to instantiate the hole
      | (Constructor, Cast(d', t1, t2))
          when Some((d', None)) == h && TypSlice.is_unknown(t1) =>
        Some((d', Some(t2))) // Note: t1 should always be unknown, but checking to be safe
      | (Constructor, _)
      | (Value, _) => h
      | (Indet, _) => combine(h, Some((d, None)))
      };
    };

  let (and.):
    (requirements('a, 'c => 'b), requirement('c)) =>
    requirements(('a, 'c), 'b) =
    ((h1, a, cb), (h2, c)) => (combine(h1, h2), (a, c), cb(c));

  let update_test = (_, _, _) => ();
};

module Instantiator = Transition(InstantiatorEVMode);

let rec instantiate' = (~in_closure=?, state, env, d) =>
  Instantiator.transition(instantiate', ~in_closure?, state, env, d);

// TODO: for the stepper add logging: i.e. return a sequence of exps paired with a step-kind detailing the instantiation made. Slice info is available for this
let instantiate = (env, d) => {
  let env = ClosureEnvironment.of_environment(env);
  switch (instantiate'((), env, d)) {
  | None
  | Some((_, None)) => d |> Util.Sequence.singleton
  | Some((hole, Some(slc))) =>
    Instantiation.(construct(DHExp.rep_id(hole), slc) |> subst(d))
  };
};
