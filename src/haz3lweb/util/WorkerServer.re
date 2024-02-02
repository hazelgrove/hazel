open Sexplib.Std;
open Haz3lcore;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (sexp, yojson)]
  type value = DHExp.t;
  [@deriving (sexp, yojson)]
  type t = (key, value);
  type u = string;

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module Response = {
  [@deriving (sexp, yojson)]
  type value = ModelResult.evaluation;
  [@deriving (sexp, yojson)]
  type t = (key, value);

  let serialize = r => r |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

let work = (d: Request.value): Response.value =>
  switch (Interface.evaluate(~settings=CoreSettings.on, d)) {
  | r => ResultOk(r)
  | exception (Interface.EvalError(error)) =>
    ResultFail(Program_EvalError(error))
  | exception Interface.DoesNotElaborate =>
    ResultFail(Program_DoesNotElaborate)
  };

let on_request = (req: Request.u) => {
  req
  |> Request.deserialize
  |> (((k, v)) => (k, work(v)))
  |> Response.serialize
  |> Js_of_ocaml.Worker.post_message;
};
let start = () => {
  Js_of_ocaml.Worker.set_onmessage(on_request);
};
