open Util;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = Haz3lcore.Exp.t;
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module Response = {
  [@deriving (show, sexp, yojson)]
  type value =
    Result.t(
      (Haz3lcore.ProgramResult.Result.t, Haz3lcore.EvaluatorState.t),
      Haz3lcore.ProgramResult.error,
    );
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let serialize = r => r |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

let work = (res: Request.value): Response.value =>
  switch (
    Haz3lcore.Evaluator.evaluate'(Haz3lcore.Builtins.env_init, {d: res})
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
  | (state, result) => Ok((result, state))
  };

let on_request = (req: string): unit =>
  req
  |> Request.deserialize
  |> List.map(((k, v)) => (k, work(v)))
  |> Response.serialize
  |> Js_of_ocaml.Worker.post_message;

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
