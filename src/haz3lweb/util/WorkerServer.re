open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (sexp, yojson)]
  type value = Haz3lcore.ModelResults.t;
  [@deriving (sexp, yojson)]
  type t = value;

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module Response = {
  [@deriving (sexp, yojson)]
  type value = Haz3lcore.ModelResults.t;
  [@deriving (sexp, yojson)]
  type t = value;

  let serialize = r => r |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

let work = (res: Request.value): Response.value =>
  Haz3lcore.ModelResults.run_pending(
    ~settings=Haz3lcore.CoreSettings.on,
    res,
  );

let on_request = (req: string): unit =>
  req
  |> Request.deserialize
  |> work
  |> Response.serialize
  |> Js_of_ocaml.Worker.post_message;

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
