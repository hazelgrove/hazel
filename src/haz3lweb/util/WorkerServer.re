open Util;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = Haz3lcore.Exp.t;
  [@deriving (show, sexp, yojson)]
  type t = list((string, value, bool, int));

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module Response = {
  [@deriving (show, sexp, yojson)]
  type value =
    Result.t(
      (Haz3lcore.Exp.t, Haz3lcore.IndetEvaluatorState.t),
      Haz3lcore.ProgramResult.error,
    );
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let serialize = r => r |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module BDFS =
  Haz3lcore.Nondeterminism.Bounded(
    (val Haz3lcore.Nondeterminism.const_incr_config(~init=100, ~inc=50)),
  );
module DFS = Haz3lcore.Nondeterminism.DFS;
module BFS = Haz3lcore.Nondeterminism.BFS;
open Haz3lcore.IndetEvaluator.Make(DFS);
let work = (res: Request.value, search, n): Response.value =>
  switch (
    res
    |> Haz3lcore.(
         search
           ? cast_errors(
               ~env=Builtins.env_init,
               ~state=IndetEvaluatorState.init,
             )
           : values(~env=Builtins.env_init, ~state=IndetEvaluatorState.init)
       )
    |> DFS.run_n(~solutions=n + 1)
    |> (l => List.nth_opt(l, n))
  ) {
  | exception (Haz3lcore.EvaluatorError.Exception(reason)) =>
    print_endline(
      "EvaluatorError:" ++ Haz3lcore.EvaluatorError.show(reason),
    );
    Error(Haz3lcore.ProgramResult.EvaulatorError(reason));
  | exception exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(
      Haz3lcore.ProgramResult.UnknownException(Printexc.to_string(exn)),
    );
  | None =>
    Error(Haz3lcore.ProgramResult.EvaulatorError(NoMoreInstantiations(res)))
  | Some((state, result)) => Ok((result, state))
  };

let on_request = (req: string): unit =>
  req
  |> Request.deserialize
  |> List.map(((k, v, b, n)) => (k, work(v, b, n)))
  |> Response.serialize
  |> Js_of_ocaml.Worker.post_message;

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
