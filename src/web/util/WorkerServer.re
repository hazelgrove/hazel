open Util;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    targets: Language.Sample.targets,
    /* Projected statics data used by the incremental driver to look up
     * per-id sub-elaborations and co-ctxs. We ship this slice instead of
     * the full StaticsBase.Map.t because the full map transitively contains
     * LivelitCtx entries that embed OCaml closures, which the structured-
     * clone algorithm postMessage uses rejects. Pass the empty slice to
     * opt out of incremental reuse. */
    eval_info_map: Language.EvalInfoMap.t,
    /* Previous run's incremental map; pass IncrEval.empty on first run. */
    prev: Language.IncrEval.t,
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

/* A wire protocol is an encode/decode pair for each direction that turns a
 * live Request.t/Response.t into some `request`/`response` form crossing the
 * worker boundary. The forms are abstract so only encode/decode can produce
 * them and the two directions can't be swapped. `name` labels the variant in
 * the Wire Metrics debug panel. Several implementations follow; the one wired
 * into WorkerClient/on_request is the active protocol, the rest exist to be
 * benchmarked against it (see WireMetrics). */
module type WIRE = {
  let name: string;
  type request;
  type response;
  let encode_request: Request.t => request;
  let decode_request: request => Request.t;
  let encode_response: Response.t => response;
  let decode_response: response => Response.t;
};

/* Post the live value graph unchanged — the pre-#2368 behavior. Chrome's
 * structured-clone serializer is recursive and overflows on deep payloads, so
 * this is unsafe as an active protocol; it's kept as the baseline the metrics
 * path exercises (the overflow surfaces as a caught exception there). */
module DirectWire: WIRE = {
  let name = "direct";
  type request = Request.t;
  type response = Response.t;
  let encode_request = Fun.id;
  let decode_request = Fun.id;
  let encode_response = Fun.id;
  let decode_response = Fun.id;
};

/* Serialize with jsoo's Marshal (iterative, so depth-safe) to a flat string
 * that structured clone copies without recursing. This is the active protocol
 * (see `Wire` below) and the #2368 fix. */
module MarshalWire: WIRE = {
  let name = "marshal";
  type request = string;
  type response = string;
  let encode_request = (req: Request.t): request =>
    Marshal.to_string(req, []);
  let decode_request = (w: request): Request.t => Marshal.from_string(w, 0);
  let encode_response = (resp: Response.t): response =>
    Marshal.to_string(resp, []);
  let decode_response = (w: response): Response.t =>
    Marshal.from_string(w, 0);
};

/* Serialize via the derived sexp converters to a string. Measures the general
 * sexp layer; its converters recurse per AST level, so deep expressions
 * overflow (a known limitation, not exercised as an active protocol). */
module SexpWire: WIRE = {
  let name = "sexp";
  type request = string;
  type response = string;
  let encode_request = (req: Request.t): request =>
    Sexplib.Sexp.to_string(Request.sexp_of_t(req));
  let decode_request = (w: request): Request.t =>
    Request.t_of_sexp(Sexplib.Sexp.of_string(w));
  let encode_response = (resp: Response.t): response =>
    Sexplib.Sexp.to_string(Response.sexp_of_t(resp));
  let decode_response = (w: response): Response.t =>
    Response.t_of_sexp(Sexplib.Sexp.of_string(w));
};

/* The active protocol crossing the worker boundary (WorkerClient / on_request).
 * Swap this alias to change it; the panel benchmarks every entry in all_wires. */
module Wire = MarshalWire;

/* All wire variants, for the metrics benchmark loop (WireMetrics). */
let all_wires: list(module WIRE) = [
  (module DirectWire),
  (module MarshalWire),
  (module SexpWire),
];

let work = (req_value: Request.value): Response.value => {
  let Request.{expr, targets, eval_info_map, prev} = req_value;
  switch (
    Language.Evaluator.evaluate(
      ~targets,
      ~prev,
      ~info_map=eval_info_map,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception (Language.EvaluatorError.Exception(reason)) =>
    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exception exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
  | (result, state) =>
    /* Clear transient data before sending to avoid serializing massive
     * amounts of unnecessary data (e.g., app_args can be 100MB+). */
    Ok((result, Language.EvaluatorState.clear_transient(state)))
  };
};

let on_request = (req: Wire.request): unit => {
  let resp: Response.t =
    Wire.decode_request(req) |> List.map(((k, v)) => (k, work(v)));
  Js_of_ocaml.Worker.post_message(Wire.encode_response(resp));
};

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
