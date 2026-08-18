/* Data for the "Evaluation" debug panel: the eval Web Worker round trip.
 * Per request we record the wall-clock latency from postMessage to onmessage,
 * the worker's own evaluation time (reported back in the result), the outcome,
 * and the encoded payload sizes (cheap — the byte length the active encoding
 * already computes, no heap walk). Rows correlate with the Worker Messaging
 * panel by the shared request id.
 *
 * Gating and the bounded history come from Metrics.Make, so every recorder here
 * is a no-op while the panel is closed and no call site tests for it. */

type status =
  | Pending
  | Success
  | Failure
  | Timeout;

type record = {
  id: int,
  entries: int, /* number of request entries (cells) */
  sent_at: float, /* precise_timestamp ms at post; used to derive latency */
  latency: option(Core.Time_ns.Span.t),
  /* The worker's own time inside the evaluator, so the gap to `latency` is the
   * queue + result serialization + transfer. */
  eval: option(Core.Time_ns.Span.t),
  status,
  req_bytes: Core.Byte_units.t,
  resp_bytes: option(Core.Byte_units.t),
};

include Metrics.Make({
  type t = record;
  let limit = 10;
});

/* Worker restarts (a new request while one is in flight, or a timeout), shown as
 * a running total in the panel header. Like every other number in this panel it
 * only counts what happened while the panel was open. */
let restarts = ref(0);
let incr_restarts = (): unit => when_enabled(() => incr(restarts));

/* Success only if every cell of the batch evaluated without error. Derived here
 * rather than at the call site so WorkerClient stays out of the panel's
 * vocabulary. */
let status_of_response = (response: WorkerServer.Response.t): status =>
  List.for_all(
    ((_, v: WorkerServer.Response.value)) =>
      switch (v) {
      | Ok(_) => true
      | Error(_) => false
      },
    response,
  )
    ? Success : Failure;

/* Record a posted request, taking the request and its encoded payload so the id,
 * cell count, clock read and byte length are all derived (and paid for) inside
 * the gate. The latency clock starts here, which the caller keeps as close to
 * the postMessage as it can. An ack retry reposts the same request id: keep the
 * original row so latency still measures from the first post, and so no id can
 * appear twice for `update` to hit. */
let record_sent =
    (~request: WorkerServer.Request.t, ~encoded: WorkerServer.Active.request)
    : unit =>
  when_enabled(() =>
    if (!List.exists((r: record) => r.id == request.request_id, history^)) {
      push({
        id: request.request_id,
        entries: List.length(request.batch),
        sent_at: Util.JsUtil.precise_timestamp(),
        latency: None,
        eval: None,
        status: Pending,
        req_bytes: WorkerServer.Active.size_request(encoded),
        resp_bytes: None,
      });
    }
  );

/* Complete a request from the result the worker sent (its id, response and
 * evaluator time) plus the still-encoded payload it arrived in, so the outcome
 * and byte length are derived here. `now` stays a parameter: the caller reads
 * the clock at the top of its message handler, before decoding, and a decode can
 * take tens of ms — reading it here would fold that into the latency. */
let record_done =
    (
      ~now: float,
      ~encoded: WorkerServer.Active.response,
      result: WorkerServer.ServerMessage.result,
    )
    : unit =>
  update(
    (r: record) => r.id == result.request_id,
    (r: record) =>
      {
        ...r,
        latency: Some(Core.Time_ns.Span.of_ms(now -. r.sent_at)),
        eval: result.eval_time,
        status: status_of_response(result.response),
        resp_bytes: Some(WorkerServer.Active.size_response(encoded)),
      },
  );

/* Mark a request as timed out (no response arrived). */
let record_timeout = (~id: int): unit =>
  update(
    (r: record) => r.id == id,
    (r: record) =>
      {
        ...r,
        status: Timeout,
      },
  );
