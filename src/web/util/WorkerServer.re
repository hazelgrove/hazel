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

/* Candidate encodings for the worker payloads; `Marshal` is active (see
 * `Active`), the rest are benchmarked against it (WorkerMetrics). Strings and
 * `all_of_encoding` are ppx-derived — no hand-maintained name lists. */
[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type encoding =
  | Direct
  | Marshal
  | Sexp;

/* An encode/decode pair per direction; the forms are abstract so only
 * encode/decode can cross the boundary and the directions can't be swapped. */
module type ENCODING = {
  type request;
  type response;
  let encode_request: Request.t => request;
  let decode_request: request => Request.t;
  let encode_response: Response.t => response;
  let decode_response: response => Response.t;
  /* Size of the encoded form (exact for the string encodings, an estimate for
   * the identity one), reported here since only the encoding knows its form. */
  let size_request: request => Core.Byte_units.t;
  let size_response: response => Core.Byte_units.t;
};

/* Post the live value graph unchanged (pre-#2368 behavior): unsafe as the
 * active encoding since Chrome's clone overflows on deep payloads, kept as the
 * benchmark baseline (the overflow surfaces there as a caught exception). */
module DirectEncoding: ENCODING = {
  open Js_of_ocaml;
  type request = Request.t;
  type response = Response.t;
  let encode_request = Fun.id;
  let decode_request = Fun.id;
  let encode_response = Fun.id;
  let decode_response = Fun.id;
  /* No serialized form, so estimate the in-memory footprint by an iterative
   * walk with a visited set (a relative measure, not exact bytes). */
  let size_fn =
    Js.Unsafe.pure_js_expr(
      {|(function (root) {
           var seen = new Set(), stack = [root], total = 0;
           while (stack.length) {
             var v = stack.pop();
             if (v === null || v === undefined) continue;
             var t = typeof v;
             if (t === 'number') { total += 8; }
             else if (t === 'string') { total += v.length; }
             else if (t === 'boolean') { total += 4; }
             else if (t === 'object') {
               if (seen.has(v)) continue;
               seen.add(v);
               if (ArrayBuffer.isView(v)) { total += v.byteLength; }
               else if (Array.isArray(v)) {
                 total += 8 * v.length;
                 for (var i = 0; i < v.length; i++) stack.push(v[i]);
               } else {
                 var ks = Object.keys(v);
                 for (var i = 0; i < ks.length; i++) {
                   total += ks[i].length + 8;
                   stack.push(v[ks[i]]);
                 }
               }
             }
           }
           return total;
         })|},
    );
  let size: 'a. 'a => Core.Byte_units.t =
    x =>
      Core.Byte_units.of_bytes_int(
        Js.Unsafe.fun_call(size_fn, [|Js.Unsafe.inject(x)|]),
      );
  let size_request = (r: request) => size(r);
  let size_response = (r: response) => size(r);
};

/* jsoo Marshal (iterative → depth-safe) to a flat string clone copies without
 * recursing. The active encoding and the #2368 fix. */
module MarshalEncoding: ENCODING = {
  type request = string;
  type response = string;
  let encode_request = (req: Request.t): request =>
    Marshal.to_string(req, []);
  let decode_request = (w: request): Request.t => Marshal.from_string(w, 0);
  let encode_response = (resp: Response.t): response =>
    Marshal.to_string(resp, []);
  let decode_response = (w: response): Response.t =>
    Marshal.from_string(w, 0);
  let size_request = (w: request) =>
    Core.Byte_units.of_bytes_int(String.length(w));
  let size_response = (w: response) =>
    Core.Byte_units.of_bytes_int(String.length(w));
};

/* Serialize via the derived sexp converters; recurses per AST level, so deep
 * expressions overflow — a benchmark comparison only. */
module SexpEncoding: ENCODING = {
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
  let size_request = (w: request) =>
    Core.Byte_units.of_bytes_int(String.length(w));
  let size_response = (w: response) =>
    Core.Byte_units.of_bytes_int(String.length(w));
};

/* Behavior for an encoding tag (exhaustive — a new variant must be added). */
let module_of_encoding = (e: encoding): (module ENCODING) =>
  switch (e) {
  | Direct => (module DirectEncoding)
  | Marshal => (module MarshalEncoding)
  | Sexp => (module SexpEncoding)
  };

/* The active encoding on the boundary (WorkerClient / on_request); swap to
 * change it. */
module Active = MarshalEncoding;

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
     * amounts of unnecessary data (e.g., app_data can be 100MB+). */
    Ok((result, Language.EvaluatorState.clear_transient(state)))
  };
};

let on_request = (req: Active.request): unit => {
  let resp: Response.t =
    Active.decode_request(req) |> List.map(((k, v)) => (k, work(v)));
  Js_of_ocaml.Worker.post_message(Active.encode_response(resp));
};

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
