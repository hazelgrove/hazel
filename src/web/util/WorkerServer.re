open Util;
module Js = Js_of_ocaml.Js;

[@deriving (show, sexp, yojson)]
type key = string;

module Request = {
  /* The incremental cache is WORKER-RESIDENT (keyed per batch key):
     shipping the whole previous cache with every request was a
     historical artifact of the ephemeral-worker era (PR #2222) — it
     dominated the request payload and once overflowed structured
     clone (#2368). `UseResident` tells the worker to use its own
     cache for this key; a stale or missing resident cache is
     CORRECTNESS-SAFE (reuse_check re-verifies every entry) and only
     costs a colder eval. `Seed` is for callers that own their cache
     (the sync main-thread path, tests). */
  [@deriving (show, sexp, yojson)]
  type prev_source =
    | UseResident
    | Seed(Language.EvaluatorState.incr_eval);
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    eval_info_map: Language.EvalInfo.t,
    prev: prev_source,
  };
  [@deriving (show, sexp, yojson)]
  type batch = list((key, value));
  [@deriving (show, sexp, yojson)]
  type t = {
    request_id: int,
    batch,
  };
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
  type t =
    | Evaluate(Request.t);
};

module ServerMessage = {
  /* Reuse-pass predictions sent before evaluation slices begin. */
  [@deriving (show, sexp, yojson)]
  type reuse_predictions =
    list((key, Language.IncrEval.t(Language.EvaluatorState.t)));

  /* Cache entries completed so far for one batch item. */
  [@deriving (show, sexp, yojson)]
  type stream_update = Language.IncrEval.outbox(Language.EvaluatorState.t);

  /* Instant liveness ping — must not do ReusePass or other batch work. */
  [@deriving (show, sexp, yojson)]
  type ack = {request_id: int};

  /* Predicted reusable cache entries for initializing streamed state. */
  [@deriving (show, sexp, yojson)]
  type reuse_plan = {
    request_id: int,
    initial: reuse_predictions,
  };

  [@deriving (show, sexp, yojson)]
  type stream = {
    request_id: int,
    key,
    update: stream_update,
  };

  [@deriving (show, sexp, yojson)]
  type result = {
    request_id: int,
    response: Response.t,
  };

  [@deriving (show, sexp, yojson)]
  type t =
    | Ack(ack)
    | ReusePlan(reuse_plan)
    | Stream(stream)
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

let error_response = exn =>
  switch (exn) {
  | Language.EvaluatorError.Exception(reason) =>
    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
  };

/* the worker's own per-key incremental caches (async path) */
let resident: Hashtbl.t(key, Language.EvaluatorState.incr_eval) =
  Hashtbl.create(4);

let resolve_prev =
    (~key: option(key)=?, prev: Request.prev_source)
    : Language.EvaluatorState.incr_eval =>
  switch (prev, key) {
  | (Seed(p), _) => p
  | (UseResident, Some(k)) =>
    Option.value(
      Hashtbl.find_opt(resident, k),
      ~default=Language.IncrEval.empty,
    )
  | (UseResident, None) => Language.IncrEval.empty
  };

let store_resident = (key: key, response: Response.value): unit =>
  switch (response) {
  | Ok((_, state)) =>
    Hashtbl.replace(resident, key, state.Language.EvaluatorState.incr_eval)
  | Error(_) => ()
  };

let evaluate_sync = (req_value: Request.value): Response.value => {
  let Request.{expr, eval_info_map, prev} = req_value;
  switch (
    Language.Evaluator.evaluate(
      ~prev=resolve_prev(prev),
      ~eval_info=eval_info_map,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception exn => error_response(exn)
  | (result, state) => Ok((result, state))
  };
};

type evaluation_start =
  | Yielding(Language.Evaluator.yielding_evaluation)
  | CompletedImmediately(Response.value);

type running = {
  request_id: int,
  key,
  remaining: Request.batch,
  completed: Response.t,
  evaluation: Language.Evaluator.yielding_evaluation,
};

type runtime =
  | Idle
  | Planning
  | Starting
  | Running(running);

type model = {
  latest_request: option(Request.t),
  runtime,
  slice_already_scheduled: bool,
};

let slice_step_budget = 5000;
/* Cumulative trampoline-step backstop across slices for a single batch item.
 * Wall-clock timeout + worker terminate is the primary runaway guard; this
 * stops pathological cases that somehow evade the client timer.
 *
 * Units are trampoline TRANSITIONS (one per Bind/Next/Done), not evaluator
 * steps — hundreds of transitions per reduction, roughly 1M transitions ≈ 2s
 * of sliced evaluation in-browser. The backstop must sit well beyond the 20s
 * client timer or ordinary heavy programs (e.g. range(1,5000)) get killed at
 * ~2s and surface as spurious "Evaluation timed out". */
let total_step_limit = 100_000_000;
let initial_model = {
  latest_request: None,
  runtime: Idle,
  slice_already_scheduled: false,
};

/* Worker execution model (three phases per request):
 * 1. Instant `Ack` — liveness only; no ReusePass.
 * 2. `ReusePlan` — predicted reusable entries for streamed state.
 * 3. Eval slices — `Stream` updates, then `Result`.
 * A newer request replaces `latest_request`; the next slice abandons stale work. */

let predict_reuse_for_request = ((key, req_value): (key, Request.value)) => {
  let Request.{expr, eval_info_map, prev} = req_value;
  let stream =
    switch (
      Language.ReusePass.reuse_pass(
        ~prev=resolve_prev(~key, prev),
        ~eval_info=eval_info_map,
        ~env=Language.Builtins.env_init,
        expr,
      )
    ) {
    | exception _ => Language.IncrEval.empty
    | stream => stream
    };
  (key, stream);
};

let stream_min_interval_ms: ref(float) = ref(100.);

let start_evaluation = (~key: key, req_value: Request.value): evaluation_start => {
  let Request.{expr, eval_info_map, prev} = req_value;
  /* stream cadence scales with program size: each posted chunk costs
     the client a stream-collection + recalc cycle that grows with the
     program (mega-2k ≈ 0.6-1s per chunk), so a fixed 100ms interval
     drowned the main thread. Clamped to [100ms, 1s]. */
  stream_min_interval_ms :=
    max(
      100.,
      min(
        1000.,
        float_of_int(Util.Id.Map.cardinal(eval_info_map.statics)) /. 12.,
      ),
    );
  switch (
    Language.Evaluator.start_yielding_evaluation(
      ~prev=resolve_prev(~key, prev),
      ~eval_info=eval_info_map,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception exn => CompletedImmediately(error_response(exn))
  | evaluation => Yielding(evaluation)
  };
};

let is_latest = (model, request_id) =>
  switch (model.latest_request) {
  | Some({request_id: latest_request_id, _}) =>
    request_id == latest_request_id
  | None => false
  };

/* All worker→client messages cross postMessage in the Active encoding
 * (see ENCODING above): posting the live value graph overflows Chrome's
 * structured clone on deep results (#2368). */
let post_message = (msg: ServerMessage.t): unit =>
  Js_of_ocaml.Worker.post_message(Active.encode_response(msg));

let post_batch_result = (model, request_id, completed) =>
  if (is_latest(model, request_id)) {
    post_message(
      ServerMessage.Result({
        request_id,
        response: List.rev(completed),
      }),
    );
  };

/* Stream chunks cross to the MAIN thread, whose consumers read only
   entry KEYS (pending-eval worklist), [seq] (frontier ordering), and
   each state's probes/tests/steps (stream collection). The
   reuse-cache payload — prev_elab (the region's whole elaborated
   subtree), prev_reuse_map, prev_probe_targets, the region's value,
   and the state's own nested incr_eval — stays worker-side
   (store_resident keeps the full response); shipping it decoded to
   ~90MB live on mega programs, most of the per-edit heap churn. */
let slim_hole: Lazy.t(Language.Exp.t) = lazy(Language.Exp.fresh(EmptyHole));
let slim_state = (state: Language.EvaluatorState.t) =>
  Language.EvaluatorState.{
    ...state,
    incr_eval: Language.IncrEval.empty,
  };
let slim_stream_update =
    (u: Language.IncrEval.outbox(Language.EvaluatorState.t))
    : Language.IncrEval.outbox(Language.EvaluatorState.t) =>
  Language.IncrEval.{
    completed: {
      entries:
        Id.Map.map(
          (e: Language.IncrEval.entry(Language.EvaluatorState.t)) =>
            Language.IncrEval.{
              prev_elab: Lazy.force(slim_hole),
              prev_reuse_map: Language.IncrEval.empty_reuse_map,
              prev_probe_targets:
                Language.EvalInfo.ProbeTargets(
                  Language.SubexpProbeTargets.empty,
                ),
              value: Lazy.force(slim_hole),
              state: slim_state(e.state),
              seq: e.seq,
            },
          u.completed.entries,
        ),
    },
    current:
      Option.map(
        (c: Language.IncrEval.current(Language.EvaluatorState.t)) =>
          Language.IncrEval.{
            ...c,
            state: slim_state(c.state),
          },
        u.current,
      ),
  };

let post_stream_update =
    (
      ~allow_empty=false,
      model,
      request_id,
      key,
      update: Language.IncrEval.outbox(Language.EvaluatorState.t),
    ) =>
  if (is_latest(model, request_id)
      && (allow_empty || !Language.IncrEval.outbox_is_empty(update))) {
    post_message(
      ServerMessage.Stream({
        request_id,
        key,
        update: slim_stream_update(update),
      }),
    );
  };

/* Stream posts are THROTTLED: every posted update costs the client a
   full update/calculate/render cycle, and un-throttled per-slice posts
   flooded mega programs with hundreds of chunks (each ~O(program) on
   the main thread). Undrained entries keep accumulating in the
   evaluation's outbox; completion flushes unconditionally. */
let last_stream_post: ref(float) = ref(0.);

let flush_stream_update = (~force=false, model, request_id, key, evaluation) => {
  let now: float = Js.Unsafe.global##.Date##now();
  if (force || now -. last_stream_post^ >= stream_min_interval_ms^) {
    last_stream_post := now;
    let update = Language.Evaluator.drain_streaming_outbox(evaluation);
    post_stream_update(model, request_id, key, update);
  };
};

/* ACK must be cheap: the client treats missing ACK as a dead worker and will
 * terminate/respawn. ReusePass belongs in `ReusePlan`, not here. */
let post_ack = (request: Request.t) =>
  post_message(ServerMessage.Ack({request_id: request.request_id}));

let post_reuse_plan = (model, request: Request.t) =>
  if (is_latest(model, request.request_id)) {
    post_message(
      ServerMessage.ReusePlan({
        request_id: request.request_id,
        initial: List.map(predict_reuse_for_request, request.batch),
      }),
    );
  };

/* Dom_html.window is unavailable in a worker, so go through the global
 * object for setTimeout. */
let schedule_async = callback =>
  ignore(Js.Unsafe.global##setTimeout(Js.wrap_callback(callback), 0.));

/* The result VALUE can embed closure ENVIRONMENTS that reference most
   of the program's runtime state — a Mod-rooted program's value is
   the module exports tuple, ~18MB marshaled at 1k lines, decoded on
   the MAIN thread (a one-time multi-second stall). Environments are
   display-opaque (the result strip never prints them; the stepper
   re-evaluates from the elab), so strip them before shipping. The
   env is replaced BEFORE the recursive descent, so the walk never
   enters the (shared, program-sized) environment structures. */
let prune_closure_envs = (e: Language.Exp.t): Language.Exp.t =>
  Language.Exp.map_term(
    ~f_exp=
      (cont, e: Language.Exp.t) =>
        switch (e.term) {
        | Closure(_, body) =>
          cont({
            ...e,
            term: Closure(Language.Environment.empty, body),
          })
        | _ => cont(e)
        },
    e,
  );

/* ... and cap the value's SIZE: the main thread only ever displays a
   budget-pruned copy (EvalResult.prune_for_display), so anything past
   the budget is marshal/decode dead weight — a Mod-rooted program's
   value (the module exports tuple, full member ASTs) added a
   ~300-400ms decode frame to EVERY edit's result arrival. Budget
   matches the display side; over-budget subtrees become holes. */
/* slightly ABOVE the display budget (EvalResult.display_budget), so
   the main side can detect ship-side truncation: its own display
   prune trips exactly when this one did */
let value_ship_budget = 6_000;
let prune_value_size = (e: Language.Exp.t): Language.Exp.t => {
  let (pruned, truncated) =
    Language.TermPrune.prune(~budget=value_ship_budget, e);
  if (truncated) {
    print_endline(
      Printf.sprintf(
        "[worker] result value exceeds %d nodes: truncated for shipping (elided parts shown as holes)",
        value_ship_budget,
      ),
    );
  };
  pruned;
};

/* The UI never consumes the incremental cache from ASYNC responses:
   the next request's prev is WORKER-RESIDENT and reuse predictions
   arrive via ReusePlan. Strip it AFTER store_resident so the
   completion payload doesn't marshal the whole entry map back across
   the boundary (it rivals the old request-side prev-cache in size). */
let slim_response = (response: Response.value): Response.value =>
  switch (response) {
  | Ok((exp, state)) =>
    Ok((
      exp |> prune_closure_envs |> prune_value_size,
      Language.EvaluatorState.{
        ...state,
        incr_eval: Language.IncrEval.empty,
      },
    ))
  | Error(_) as e => e
  };

let rec evaluate_next_batch_item = (model, request_id, completed, remaining) =>
  switch (remaining) {
  | [] =>
    let model = {
      ...model,
      runtime: Idle,
    };
    post_batch_result(model, request_id, completed);
    model;
  | [(key, req_value), ...remaining] =>
    switch (start_evaluation(~key, req_value)) {
    | CompletedImmediately(response) =>
      store_resident(key, response);
      evaluate_next_batch_item(
        model,
        request_id,
        [(key, slim_response(response)), ...completed],
        remaining,
      );
    | Yielding(evaluation) =>
      let model = {
        ...model,
        runtime:
          Running({
            request_id,
            key,
            remaining,
            completed,
            evaluation,
          }),
      };
      model;
    }
  }
and begin_latest_batch = model =>
  switch (model.latest_request) {
  | None => {
      ...model,
      runtime: Idle,
    }
  | Some({request_id, batch}) =>
    evaluate_next_batch_item(model, request_id, [], batch)
  }
and finish_current_item = (model, running, response) => {
  store_resident(running.key, response);
  evaluate_next_batch_item(
    model,
    running.request_id,
    [(running.key, slim_response(response)), ...running.completed],
    running.remaining,
  );
}
and plan_latest_batch = model =>
  switch (model.latest_request) {
  | None => {
      ...model,
      runtime: Idle,
    }
  | Some(request) =>
    post_reuse_plan(model, request);
    {
      ...model,
      runtime: Starting,
    };
  }
and run_scheduled_slice = model => {
  let model = {
    ...model,
    slice_already_scheduled: false,
  };
  switch (model.runtime) {
  | Idle => model
  | Planning => plan_latest_batch(model)
  | Starting => begin_latest_batch(model)
  | Running(running) when !is_latest(model, running.request_id) =>
    plan_latest_batch(model)
  | Running(running) =>
    switch (
      Language.Evaluator.run_yielding_slice(
        ~step_budget=slice_step_budget,
        running.evaluation,
      )
    ) {
    | exception exn =>
      finish_current_item(model, running, error_response(exn))
    | EvaluationCompleted(value) =>
      flush_stream_update(
        ~force=true,
        model,
        running.request_id,
        running.key,
        running.evaluation,
      );
      finish_current_item(model, running, Ok(value));
    | EvaluationYielded(evaluation) =>
      flush_stream_update(model, running.request_id, running.key, evaluation);
      if (Language.Evaluator.yielding_step_count(evaluation)
          >= total_step_limit) {
        finish_current_item(
          model,
          running,
          Error(Language.ProgramResult.Timeout),
        );
      } else {
        let model = {
          ...model,
          runtime:
            Running({
              ...running,
              evaluation,
            }),
        };
        model;
      };
    }
  };
};

let install_message_handler = () => {
  let model = ref(initial_model);

  let rec commit = next_model => {
    let should_schedule_slice =
      switch (next_model.runtime) {
      | Idle => false
      | Planning
      | Starting
      | Running(_) => !next_model.slice_already_scheduled
      };
    model :=
      should_schedule_slice
        ? {
          ...next_model,
          slice_already_scheduled: true,
        }
        : next_model;
    if (should_schedule_slice) {
      schedule_async(() => commit(run_scheduled_slice(model^)));
    };
  };

  let on_request = (req: Active.request): unit => {
    let ClientMessage.Evaluate(request) = Active.decode_request(req);
    post_ack(request);
    commit({
      ...model^,
      latest_request: Some(request),
      runtime: Planning,
    });
  };

  Js_of_ocaml.Worker.set_onmessage(on_request);
};
