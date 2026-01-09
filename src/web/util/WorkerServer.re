open Util;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    probe_map: Id.Map.t(Language.Probe.t),
  };
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
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

  let serialize = r => r |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

let work = (req_value: Request.value): Response.value => {
  let Request.{expr, probe_map} = req_value;
  let eval_start = JsUtil.precise_timestamp();
  let result =
    switch (
      Language.Evaluator.evaluate(
        ~probe_map,
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
  let eval_end = JsUtil.precise_timestamp();
  Printf.printf("  Eval only (ms): %.2f\n", eval_end -. eval_start);
  result;
};

let on_request = (req: string): unit =>
  req
  |> Request.deserialize
  |> List.map(((k, v)) => (k, work(v)))
  |> Response.serialize
  |> Js_of_ocaml.Worker.post_message;

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
