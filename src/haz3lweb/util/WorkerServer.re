open Util;
open Js_of_ocaml;

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

let pending = ref("");
let is_running = ref(false);

let work = (req: Request.value): Response.value => {
  Haz3lcore.ModelResults.run_pending(
    ~settings=Haz3lcore.CoreSettings.on,
    req,
  );
};

let handle = () => {
  let req = Request.deserialize(pending^);
  let res = work(req);
  let out = Response.serialize(res);
  Js_of_ocaml.Worker.post_message(out);
  is_running := false;
};

Js.Unsafe.global##.jscode := [%js {as _; pub handle = handle}];

let on_request = (req: string): unit => {
  pending := req;
  if (! is_running^) {
    is_running := true;
    Js.Unsafe.eval_string("setTimeout(jscode.handle(), 0)");
  };
};

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
