/* Collector for the "Evaluation" debug panel; see EvalMetrics.re. Recorders are
 * self-gating, so callers never check whether the panel is open. */

type status =
  | Pending
  | Success
  | Failure
  | Timeout;

type record = {
  id: int,
  entries: int,
  sent_at: float,
  latency: option(Core.Time_ns.Span.t),
  eval: option(Core.Time_ns.Span.t),
  status,
  req_bytes: Core.Byte_units.t,
  resp_bytes: option(Core.Byte_units.t),
};

/* Turn collection on/off from settings; called once per update cycle. */
let sync: (~enabled: bool) => unit;

/* Recent requests, newest first. */
let history: ref(list(record));

/* Worker restarts observed while the panel was open. */
let restarts: ref(int);

let record_sent:
  (~request: WorkerServer.Request.t, ~encoded: WorkerServer.Active.request) =>
  unit;

/* `now` is read by the caller before it decodes the response; see EvalMetrics.re. */
let record_done:
  (
    ~now: float,
    ~encoded: WorkerServer.Active.response,
    WorkerServer.ServerMessage.result
  ) =>
  unit;

let record_timeout: (~id: int) => unit;

let incr_restarts: unit => unit;
