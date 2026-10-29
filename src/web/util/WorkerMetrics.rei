/* Collector for the "Worker Messaging" debug panel; see WorkerMetrics.re. The
 * recorders are self-gating, so callers never check whether the panel is open. */

type dir_metric = {
  encoding: WorkerServer.encoding,
  encode: option(Core.Time_ns.Span.t),
  clone: option(Core.Time_ns.Span.t),
  decode: option(Core.Time_ns.Span.t),
  size: option(Core.Byte_units.t),
  error: option(string),
};

type record = {
  id: int,
  entries: int,
  request: list(dir_metric),
  response: list(dir_metric),
};

/* Turn collection on/off from settings; called once per update cycle. */
let sync: (~enabled: bool) => unit;

/* Which encodings the panel benchmarks; synced from settings alongside `sync`. */
let set_encodings: list(WorkerServer.encoding) => unit;

/* Recent requests, newest first. */
let history: ref(list(record));

/* Benchmark the request-side encodings, opening a row keyed by the request id. */
let record_request: (int, WorkerServer.ClientMessage.t) => unit;

/* Benchmark the response-side encodings onto that request's row. */
let record_response: (int, WorkerServer.ServerMessage.t) => unit;
