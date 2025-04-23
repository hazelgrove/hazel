open Util;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = Semantics.Exp.t;
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module Response = {
  [@deriving (show, sexp, yojson)]
  type value =
    Result.t(
      (Semantics.Exp.t, Semantics.EvaluatorState.t),
      Semantics.ProgramResult.error,
    );
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let serialize = r => r |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

let work = (res: Request.value): Response.value =>
  switch (Semantics.Evaluator.evaluate(~env=Semantics.Builtins.env_init, res)) {
  | exception (Semantics.EvaluatorError.Exception(reason)) =>
    print_endline(
      "EvaluatorError:" ++ Semantics.EvaluatorError.show(reason),
    );
    Error(Semantics.ProgramResult.EvaulatorError(reason));
  | exception exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(
      Semantics.ProgramResult.UnknownException(Printexc.to_string(exn)),
    );
  | (result, state) => Ok((result, state))
  };

let on_request = (req: string): unit =>
  req
  |> Request.deserialize
  |> List.map(((k, v)) => (k, work(v)))
  |> Response.serialize
  |> Js_of_ocaml.Worker.post_message;

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
