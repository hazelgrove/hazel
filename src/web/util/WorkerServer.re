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

/* Marshaled wire forms of the worker payloads.
 *
 * post_message structured-clones its argument, and Chrome's clone serializer
 * is recursive: it overflows on deep (but finite, acyclic) payloads (#2368).
 * jsoo's Marshal is iterative and yields a flat string that clones without
 * recursing. The payloads are closure-free (clone rejects closures yet
 * succeeds here before overflowing), so Marshal without the unsupported
 * Closures flag is safe.
 *
 * The types are abstract so the boundary stays typed Worker.worker(request,
 * response), not (string, string): only the encode/decode functions can cross
 * it and the two directions can't be swapped. */
module Wire: {
  type request;
  type response;
  let encode_request: Request.t => request;
  let decode_request: request => Request.t;
  let encode_response: Response.t => response;
  let decode_response: response => Response.t;
} = {
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
