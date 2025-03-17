open ProgramResult.Result;

open Util.Sequence;

open Transition;

// Checks if the term is stuck due to being a failed cast
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

  let (let.): (requirements('a, DHExp.t), 'a => rule) => result =
    ((h, a, d), rl) => {
      switch (rl(a), Exp.term_of(d)) {
      | (Indet, FailedCast(_, _, _))
      | (IndetMatch(_), FailedCast(_, _, _)) => true
      | (Step(_), _) =>
        failwith("Step possible before cast failure checking") // Assume full reduction before instantiation
      // Pattern match on casts to retrieve the type to instantiate the hole
      | (Constructor, _)
      | (Value, _)
      | (Indet, _)
      | (IndetMatch(_), _) => h
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
  CastErrChecker.transition(contains_error, ~in_closure?, state, env, d);

let contains_error = contains_error((), ClosureEnvironment.empty);

let evaluate' = (env, d) =>
  d |> IndetEvaluator.evaluate'(env) |> Futures.filter(~f=contains_error);

let evaluate =
    (~settings: CoreSettings.t, ~env=Builtins.env_init, elab: DHExp.t)
    : ProgramResult.t(ProgramResult.indet) =>
  switch () {
  | _ when !settings.dynamics => Off(elab)
  | _ =>
    switch (evaluate'(env, elab)) {
    // TODO: Catch exceptions during instantiation, so that instantiation specific exception can be distinguished
    | exception (EvaluatorError.Exception(reason)) =>
      print_endline("EvaluatorError:" ++ EvaluatorError.show(reason));
      ResultFail(EvaulatorError(reason));
    | exception exn =>
      print_endline("EXN:" ++ Printexc.to_string(exn));
      ResultFail(UnknownException(Printexc.to_string(exn)));
    | results => ResultOk({results: results})
    }
  };
