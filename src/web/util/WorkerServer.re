open Util;
module Js = Js_of_ocaml.Js;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    /* Projected statics data used by the incremental driver to look up
     * per-id sub-elaborations and co-ctxs. We ship this slice instead of
     * the full StaticsBase.Map.t because the full map transitively contains
     * LivelitCtx entries that embed OCaml closures, which the structured-
     * clone algorithm postMessage uses rejects. Pass the empty slice to
     * opt out of incremental reuse. */
    eval_info_map: Language.EvalInfo.t,
    /* Previous run's incremental map; pass IncrEval.empty on first run. */
    prev: Language.EvaluatorState.incr_eval,
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

module ClientMessage = {
  [@deriving (show, sexp, yojson)]
  type evaluate = {
    request_id: int,
    batch: Request.t,
  };

  [@deriving (show, sexp, yojson)]
  type t =
    | Evaluate(evaluate);
};

module ServerMessage = {
  [@deriving (show, sexp, yojson)]
  type result = {
    request_id: int,
    response: Response.t,
  };

  [@deriving (show, sexp, yojson)]
  type t =
    | Ack(int)
    | Result(result);
};

/* Candidate encodings for the worker payloads; `Marshal` is active (see
 * `Active`), the rest are benchmarked against it (WorkerMetrics). Strings and
 * `all_of_encoding` are ppx-derived — no hand-maintained name lists. */
[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type encoding =
  | Direct
  | Marshal
  | Sexp;

/* An encode/decode pair per direction over the wire messages (the request
 * direction carries ClientMessage.t, the response direction ServerMessage.t);
 * the forms are abstract so only encode/decode can cross the boundary and the
 * directions can't be swapped. */
module type ENCODING = {
  type request;
  type response;
  let encode_request: ClientMessage.t => request;
  let decode_request: request => ClientMessage.t;
  let encode_response: ServerMessage.t => response;
  let decode_response: response => ServerMessage.t;
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
  type request = ClientMessage.t;
  type response = ServerMessage.t;
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
  let encode_request = (req: ClientMessage.t): request =>
    Marshal.to_string(req, []);
  let decode_request = (w: request): ClientMessage.t =>
    Marshal.from_string(w, 0);
  let encode_response = (resp: ServerMessage.t): response =>
    Marshal.to_string(resp, []);
  let decode_response = (w: response): ServerMessage.t =>
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
  let encode_request = (req: ClientMessage.t): request =>
    Sexplib.Sexp.to_string(ClientMessage.sexp_of_t(req));
  let decode_request = (w: request): ClientMessage.t =>
    ClientMessage.t_of_sexp(Sexplib.Sexp.of_string(w));
  let encode_response = (resp: ServerMessage.t): response =>
    Sexplib.Sexp.to_string(ServerMessage.sexp_of_t(resp));
  let decode_response = (w: response): ServerMessage.t =>
    ServerMessage.t_of_sexp(Sexplib.Sexp.of_string(w));
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
  let Request.{expr, eval_info_map, prev} = req_value;
  switch (
    Language.Evaluator.evaluate(
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
  | (result, state) => Ok((result, state))
  };
};

type started_work =
  | Started(Language.Evaluator.yielding_evaluation)
  | Finished(Response.value);

type running = {
  request_id: int,
  key,
  remaining: Request.t,
  completed: Response.t,
  evaluation: Language.Evaluator.yielding_evaluation,
};

type runtime =
  | Idle
  | Running(running);

let slice_step_budget = 5000;
let latest_request: ref(option(ClientMessage.evaluate)) = ref(None);
let runtime = ref(Idle);
let pump_scheduled = ref(false);

let error_response = exn =>
  switch (exn) {
  | Language.EvaluatorError.Exception(reason) =>
    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
  };

let finish_success = ((result, state)): Response.value =>
  Ok((result, state));

let start_work = (req_value: Request.value): started_work => {
  let Request.{expr, eval_info_map, prev} = req_value;
  switch (
    Language.Evaluator.start_yielding_evaluation(
      ~prev,
      ~info_map=eval_info_map,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception exn => Finished(error_response(exn))
  | evaluation => Started(evaluation)
  };
};

let is_latest = request_id =>
  switch (latest_request^) {
  | Some({request_id: latest_request_id, _}) =>
    request_id == latest_request_id
  | None => false
  };

/* All worker→client messages cross postMessage in the Active encoding
 * (see ENCODING above): posting the live value graph overflows Chrome's
 * structured clone on deep results (#2368). */
let post_message = (msg: ServerMessage.t): unit =>
  Js_of_ocaml.Worker.post_message(Active.encode_response(msg));

let post_result = (request_id, completed) =>
  if (is_latest(request_id)) {
    post_message(
      ServerMessage.Result({
        request_id,
        response: List.rev(completed),
      }),
    );
  };

let schedule = callback => {
  ignore(
    Js.Unsafe.meth_call(
      Js.Unsafe.global,
      "setTimeout",
      [|
        Js.Unsafe.inject(Js.wrap_callback(callback)),
        Js.Unsafe.inject(0.),
      |],
    ),
  );
};

let rec start_next = (request_id, completed, remaining) =>
  switch (remaining) {
  | [] =>
    runtime := Idle;
    post_result(request_id, completed);
  | [(key, req_value), ...remaining] =>
    switch (start_work(req_value)) {
    | Finished(response) =>
      start_next(request_id, [(key, response), ...completed], remaining)
    | Started(evaluation) =>
      runtime :=
        Running({
          request_id,
          key,
          remaining,
          completed,
          evaluation,
        });
      schedule_pump();
    }
  }
and start_latest = () =>
  switch (latest_request^) {
  | None => runtime := Idle
  | Some({request_id, batch}) => start_next(request_id, [], batch)
  }
and finish_running = (running, response) =>
  start_next(
    running.request_id,
    [(running.key, response), ...running.completed],
    running.remaining,
  )
and pump = () => {
  pump_scheduled := false;
  switch (runtime^) {
  | Idle => start_latest()
  | Running(running) when !is_latest(running.request_id) => start_latest()
  | Running(running) =>
    switch (
      Language.Evaluator.run_yielding_slice(
        ~step_budget=slice_step_budget,
        running.evaluation,
      )
    ) {
    | exception exn => finish_running(running, error_response(exn))
    | EvaluationCompleted(value) =>
      finish_running(running, finish_success(value))
    | EvaluationStepLimitExceeded =>
      finish_running(
        running,
        Error(
          Language.ProgramResult.UnknownException("Step limit exceeded"),
        ),
      )
    | EvaluationYielded(evaluation) =>
      runtime :=
        Running({
          ...running,
          evaluation,
        });
      schedule_pump();
    }
  };
}
and schedule_pump = () =>
  if (! pump_scheduled^) {
    pump_scheduled := true;
    schedule(pump);
  };

let on_request = (req: Active.request): unit => {
  let ClientMessage.Evaluate(request) = Active.decode_request(req);
  latest_request := Some(request);
  post_message(ServerMessage.Ack(request.request_id));
  schedule_pump();
};

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
