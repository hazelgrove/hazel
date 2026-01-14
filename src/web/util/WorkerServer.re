open Util;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    targets: Language.Sample.targets,
  };
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));
};

module Response = {
  [@deriving (show, sexp, yojson)]
  type value =
    Result.t(
      (Language.Exp.t, Language.EvaluatorState.t),
      Language.ProgramResult.error,
    );
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let (sexp_of_t, t_of_sexp) =
    Util.StructureShareSexp.structure_share_in(sexp_of_t, t_of_sexp);
};

let work = (req_value: Request.value): Response.value => {
  let Request.{expr, targets} = req_value;
  switch (
    Language.Evaluator.evaluate(
      ~targets,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception (Language.EvaluatorError.Exception(reason)) =>
    print_endline(
      "EvaluatorError:" ++ Language.EvaluatorError.show(reason),
    );
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exception exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(
      Language.ProgramResult.UnknownException(Printexc.to_string(exn)),
    );
  | (result, state) => Ok((result, state))
  };
};

let on_request = (req: Request.t): unit =>
  req
  |> List.map(((k, v)) => (k, work(v)))
  |> Js_of_ocaml.Worker.post_message;

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
