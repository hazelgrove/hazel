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
  (~id: int, ~entries: int, ~sent_at: float, ~req_bytes: Core.Byte_units.t) =>
  unit;

let record_done:
  (
    ~id: int,
    ~now: float,
    ~response: WorkerServer.Response.t,
    ~eval_ms: float,
    ~resp_bytes: Core.Byte_units.t
  ) =>
  unit;

let record_timeout: (~id: int) => unit;

let incr_restarts: unit => unit;
