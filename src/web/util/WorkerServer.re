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
 * that structured clone copies without recursing. The PR #2370 approach. */
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

/* Flattened wire forms of the worker payloads.
 *
 * post_message structured-clones its argument, and Chrome's clone serializer
 * is recursive: it overflows on deep (but finite, acyclic) payloads (#2368).
 * Serializing to a string (Marshal/sexp) sidesteps that but is slow for
 * large payloads, so instead we re-shape the value. jsoo represents every
 * OCaml block as a JS array, so `flatten` scans the graph once, numbering
 * blocks in discovery order, and packs it columnar:
 *
 *   lens  Float64Array  length of block i
 *   data  Float64Array  every block's fields, concatenated in block order;
 *                       a field that is a child block holds the child's
 *                       block number, a numeric leaf holds itself, and a
 *                       non-numeric leaf (string) holds a dummy
 *   refs  Float64Array  ascending data positions holding child block numbers
 *   vpos  Float64Array  ascending data positions holding non-numeric leaves
 *   vals  Array         the non-numeric leaves, paired with vpos
 *
 * The wire object is thus a fixed handful of flat arrays no matter how deep
 * the payload is, so the clone never recurses, and the numeric bulk lives
 * in typed arrays the serializer copies as raw bytes. `unflatten` rebuilds
 * the blocks in one pass, walking refs/vpos in lockstep with data. Sharing
 * is preserved (blocks are numbered by identity), so DAG-shaped payloads
 * don't expand. Non-numeric leaves must be structured-clone-safe, exactly
 * as every leaf had to be when the value graph was posted directly.
 *
 * The types are abstract so the boundary stays typed Worker.worker(request,
 * response) and only encode/decode can cross it, with the two directions
 * not swappable. */
module Wire: WIRE = {
  open Js_of_ocaml;

  let name = "columnar";

  type request = Js.Unsafe.any;
  type response = Js.Unsafe.any;

  let flatten_fn =
    Js.Unsafe.pure_js_expr(
      {|(function (root) {
           if (!Array.isArray(root)) return { imm: root };
           var seen = new Map(), work = [root];
           var lens = [root.length], data = [], refs = [], vpos = [], vals = [];
           seen.set(root, 0);
           for (var w = 0; w < work.length; w++) {
             var b = work[w];
             for (var j = 0; j < b.length; j++) {
               var f = b[j];
               if (Array.isArray(f)) {
                 var i = seen.get(f);
                 if (i === undefined) {
                   i = work.length;
                   seen.set(f, i);
                   work.push(f);
                   lens.push(f.length);
                 }
                 refs.push(data.length);
                 data.push(i);
               } else if (typeof f === 'number') {
                 data.push(f);
               } else {
                 vpos.push(data.length);
                 vals.push(f);
                 data.push(0);
               }
             }
           }
           return {
             lens: new Float64Array(lens),
             data: new Float64Array(data),
             refs: new Float64Array(refs),
             vpos: new Float64Array(vpos),
             vals: vals
           };
         })|},
    );

  let unflatten_fn =
    Js.Unsafe.pure_js_expr(
      {|(function (wire) {
           if (wire.lens === undefined) return wire.imm;
           var lens = wire.lens, data = wire.data, refs = wire.refs,
               vpos = wire.vpos, vals = wire.vals;
           var n = lens.length, blocks = new Array(n);
           for (var i = 0; i < n; i++) blocks[i] = new Array(lens[i]);
           var p = 0, r = 0, s = 0;
           for (var i = 0; i < n; i++) {
             var b = blocks[i], len = lens[i];
             for (var j = 0; j < len; j++, p++) {
               if (r < refs.length && refs[r] === p) {
                 b[j] = blocks[data[p]];
                 r++;
               } else if (s < vpos.length && vpos[s] === p) {
                 b[j] = vals[s];
                 s++;
               } else {
                 b[j] = data[p];
               }
             }
           }
           return blocks[0];
         })|},
    );

  let flatten: Js.Unsafe.any => Js.Unsafe.any =
    v => Js.Unsafe.fun_call(flatten_fn, [|v|]);
  let unflatten: Js.Unsafe.any => Js.Unsafe.any =
    w => Js.Unsafe.fun_call(unflatten_fn, [|w|]);

  let encode_request = (req: Request.t): request =>
    flatten(Js.Unsafe.inject(req));
  let decode_request = (w: request): Request.t => Obj.magic(unflatten(w));
  let encode_response = (resp: Response.t): response =>
    flatten(Js.Unsafe.inject(resp));
  let decode_response = (w: response): Response.t =>
    Obj.magic(unflatten(w));
};

/* Alternative wire (#2368 follow-up experiment): the plain-OCaml version of
 * "use an array instead of a list". Encode converts the top-level
 * request/response list to an OCaml array — which jsoo represents as one
 * wide JS array — and the result is posted directly; decode converts back.
 * Only the spine it converts gets flatter: everything inside each entry
 * still crosses as its natural nested graph, so a payload that is deep
 * *inside* an entry (a long ListLit, a deeply nested AST) still makes
 * structured clone recurse and can still overflow it (#2368). It exists to
 * measure the "arrays instead of lists" idea against Wire's columnar
 * packing, which flattens every spine at once. */
module ArrayWire: WIRE = {
  let name = "array";
  type request = array((key, Request.value));
  type response = array((key, Response.value));
  let encode_request = Array.of_list;
  let decode_request = Array.to_list;
  let encode_response = Array.of_list;
  let decode_response = Array.to_list;
};

/* All wire variants, for the metrics benchmark loop (WireMetrics). */
let all_wires: list(module WIRE) = [
  (module DirectWire),
  (module MarshalWire),
  (module SexpWire),
  (module Wire),
  (module ArrayWire),
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
