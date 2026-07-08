/* Data for the "Evaluation" debug panel: the eval Web Worker round trip.
 * Per request we record the wall-clock latency from postMessage to onmessage,
 * the outcome, and the encoded payload sizes (cheap — the byte length the
 * active encoding already computes, no heap walk). Correlated with the Worker
 * Messaging panel by the shared request id (WorkerMetrics.next_id).
 *
 * Gated by `enabled` (synced from settings in Page.Update.calculate via `sync`)
 * so nothing is recorded while the panel is closed. */

let enabled = ref(false);

let sync = (~enabled as is_enabled: bool): unit => enabled := is_enabled;

/* Worker restarts (on a new request while one is in flight, or on timeout).
   A running total, shown in the panel header. */
let restarts = ref(0);
let incr_restarts = (): unit => incr(restarts);

type status =
  | Pending
  | Ok
  | Fail
  | Timeout;

type record = {
  id: int,
  entries: int, /* number of request entries (cells) */
  sent_at: float, /* precise_timestamp ms at post; used to derive latency */
  latency: option(Core.Time_ns.Span.t),
  status,
  req_bytes: Core.Byte_units.t,
  resp_bytes: option(Core.Byte_units.t),
};

let history_limit = 10;
let history: ref(list(record)) = ref([]); /* newest first */

/* Record a posted request; the latency clock starts at `sent_at`. */
let record_sent =
    (~id: int, ~entries: int, ~sent_at: float, ~req_bytes: Core.Byte_units.t)
    : unit =>
  history :=
    [
      {
        id,
        entries,
        sent_at,
        latency: None,
        status: Pending,
        req_bytes,
        resp_bytes: None,
      },
      ...Util.ListUtil.take(history_limit - 1, history^),
    ];

/* Complete a request with its response; `now` is precise_timestamp ms. */
let record_done =
    (~id: int, ~now: float, ~status: status, ~resp_bytes: Core.Byte_units.t)
    : unit =>
  history :=
    List.map(
      (r: record) =>
        r.id == id
          ? {
            ...r,
            latency: Some(Core.Time_ns.Span.of_ms(now -. r.sent_at)),
            status,
            resp_bytes: Some(resp_bytes),
          }
          : r,
      history^,
    );

/* Mark a request as timed out (no response arrived). */
let record_timeout = (~id: int): unit =>
  history :=
    List.map(
      (r: record) =>
        r.id == id
          ? {
            ...r,
            status: Timeout,
          }
          : r,
      history^,
    );
