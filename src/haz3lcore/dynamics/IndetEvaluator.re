open ProgramResult.Result;

open Util.Sequence;

// Assumes the input ds are already evaluated
let rec evaluate = (env, ds: Futures.t): Futures.t => {
  ds
  >>| (
    d =>
      d
      |> Instantiator.instantiate(env)
      >>| Evaluator.evaluate''(env)
      |> (
        init =>
          unfold(~init, ~f=s => Some((s, s |> evaluate(env))))
          // TODO: Check if env needs to change
          |> interleave
      )
  )
  |> interleave;
};

let evaluate' = (env, d: DHExp.t): Futures.t => {
  d |> Evaluator.evaluate''(env) |> singleton |> evaluate(env);
};

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
