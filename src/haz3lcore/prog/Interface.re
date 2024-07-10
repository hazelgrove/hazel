let dh_err = (error: string): DHExp.t => BoundVar(error);

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

exception DoesNotElaborate;
let elaborate = (~settings: CoreSettings.t, map, term): DHExp.t =>
  switch () {
  | _ when !settings.statics => dh_err("Statics disabled")
  | _ when !settings.dynamics && !settings.elaborate =>
    dh_err("Dynamics & Elaboration disabled")
  | _ =>
    switch (elaborate(map, term)) {
    | DoesNotElaborate => dh_err("Elaboration returns None")
    | Elaborates(d, _, _) => d
    }
  };

let evaluate =
    (~settings: CoreSettings.t, ~env=Builtins.env_init, elab: DHExp.t)
    : ProgramResult.t =>
  switch () {
  | _ when !settings.dynamics => Off(elab)
  | _ =>
    switch (Evaluator.evaluate(env, elab)) {
    | exception (EvaluatorError.Exception(reason)) =>
      print_endline("EvaluatorError:" ++ EvaluatorError.show(reason));
      ResultFail(EvaulatorError(reason));
    | exception exn =>
      print_endline("EXN:" ++ Printexc.to_string(exn));
      ResultFail(UnknownException(Printexc.to_string(exn)));
    | (state, result) => ResultOk({result, state})
    }
  };
