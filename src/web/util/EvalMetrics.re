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

/* Record a posted request; the latency clock starts at `sent_at`. An ack retry
 * reposts the same request id: keep the original row so latency still measures
 * from the first post, and so no id can appear twice for `update` to hit. */
let record_sent =
    (~id: int, ~entries: int, ~sent_at: float, ~req_bytes: Core.Byte_units.t)
    : unit =>
  when_enabled(() =>
    if (!List.exists((r: record) => r.id == id, history^)) {
      push({
        id,
        entries,
        sent_at,
        latency: None,
        eval: None,
        status: Pending,
        req_bytes,
        resp_bytes: None,
      });
    }
  );

/* Complete a request with its response; `now` is precise_timestamp ms and
 * `eval_ms` the evaluator time the worker measured for this batch. */
let record_done =
    (
      ~id: int,
      ~now: float,
      ~response: WorkerServer.Response.t,
      ~eval_ms: float,
      ~resp_bytes: Core.Byte_units.t,
    )
    : unit =>
  update(
    (r: record) => r.id == id,
    (r: record) =>
      {
        ...r,
        latency: Some(Core.Time_ns.Span.of_ms(now -. r.sent_at)),
        eval: Some(Core.Time_ns.Span.of_ms(eval_ms)),
        status: status_of_response(response),
        resp_bytes: Some(resp_bytes),
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
