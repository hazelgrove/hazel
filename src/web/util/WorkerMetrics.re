open Js_of_ocaml;

/* Data for the "Worker Messaging" debug panel: how the main thread talks to
 * the eval Web Worker. Per-request benchmarking of the candidate encodings
 * (WorkerServer.encoding / ENCODING) that pack payloads across the boundary.
 *
 * For every enabled encoding we encode the real payload, run it through the
 * browser's structuredClone (the same serializer postMessage uses, so an
 * encoding that overflows the clone stack — e.g. Direct on a deep result,
 * #2368 — surfaces here as a caught exception), then decode, timing each stage;
 * the encoded size is reported by the encoding itself. Results feed the Worker
 * Messaging table in DebugSidebar.
 *
 * Everything runs on the main thread; nothing crosses to the worker. Gated by
 * `enabled` (synced from settings in Page.Update.calculate via `sync`) so
 * normal editing pays nothing. */

let enabled = ref(false);

/* Encodings the user has turned on in the panel (WorkerServer.encoding). Only
   enabled encodings are measured, so e.g. the slow sexp encoding can be
   skipped. */
let enabled_encodings: ref(list(WorkerServer.encoding)) = ref([]);

/* Sync the gating flags from settings; called once per update cycle from
   Page.Update.calculate (the only place with the full app settings in scope). */
let sync =
    (~enabled as is_enabled: bool, ~encodings: list(WorkerServer.encoding))
    : unit => {
  enabled := is_enabled;
  enabled_encodings := encodings;
};

let active_encodings = (): list(WorkerServer.encoding) =>
  List.filter(
    e => List.mem(e, enabled_encodings^),
    WorkerServer.all_of_encoding,
  );

/* One encoding measured in one direction. A stage's duration/size is None if it
 * didn't complete; `error` holds the message of the stage that threw (if any),
 * so a failure reads as an absence rather than a misleading 0. */
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
  entries: int, /* length of the original request/response list */
  request: list(dir_metric),
  response: list(dir_metric),
};

let history_limit = 10;
let history: ref(list(record)) = ref([]); /* newest first */

let id_counter = ref(0);
let next_id = (): int => {
  incr(id_counter);
  id_counter^;
};

/* structuredClone: the serializer postMessage applies to its argument. */
let clone_fn =
  Js.Unsafe.pure_js_expr("(function (x) { return structuredClone(x); })");
let structured_clone: 'a. 'a => 'a =
  x => Js.Unsafe.fun_call(clone_fn, [|Js.Unsafe.inject(x)|]);

let timed: 'a. (unit => 'a) => (Core.Time_ns.Span.t, 'a) =
  f => {
    let t0 = Util_web.JsUtil.precise_timestamp();
    let x = f();
    (Core.Time_ns.Span.of_ms(Util_web.JsUtil.precise_timestamp() -. t0), x);
  };

/* Measure encode -> size -> structuredClone -> decode for one direction. Each
 * stage that completes records its value; the first stage to throw sets `error`
 * and leaves it and later stages None (never a misleading 0). Size is asked of
 * the encoding via `size`. */
let measure:
  'w 'a.
  (
    ~encoding: WorkerServer.encoding,
    ~encode: unit => 'w,
    ~size: 'w => Core.Byte_units.t,
    ~decode: 'w => 'a
  ) =>
  dir_metric
 =
  (~encoding, ~encode, ~size, ~decode) => {
    let enc = ref(None)
    and cln = ref(None)
    and dec = ref(None)
    and sz = ref(None);
    let error =
      switch (
        {
          let (e, encoded) = timed(encode);
          enc := Some(e);
          sz := Some(size(encoded));
          let (c, cloned) = timed(() => structured_clone(encoded));
          cln := Some(c);
          let (d, _) = timed(() => decode(cloned));
          dec := Some(d);
        }
      ) {
      | () => None
      | exception exn => Some(Printexc.to_string(exn))
      };
    {
      encoding,
      encode: enc^,
      clone: cln^,
      decode: dec^,
      size: sz^,
      error,
    };
  };

let push = (r: record): unit =>
  history := [r, ...Util_web.ListUtil.take(history_limit - 1, history^)];

let record_request = (id: int, msg: WorkerServer.ClientMessage.t): unit => {
  let request =
    List.map(
      (e: WorkerServer.encoding) => {
        module M = (val WorkerServer.module_of_encoding(e));
        measure(
          ~encoding=e,
          ~encode=() => M.encode_request(msg),
          ~size=M.size_request,
          ~decode=M.decode_request,
        );
      },
      active_encodings(),
    );
  let WorkerServer.ClientMessage.Evaluate({batch, _}) = msg;
  push({
    id,
    entries: List.length(batch),
    request,
    response: [],
  });
};

let record_response = (id: int, resp: WorkerServer.ServerMessage.t): unit => {
  let response =
    List.map(
      (e: WorkerServer.encoding) => {
        module M = (val WorkerServer.module_of_encoding(e));
        measure(
          ~encoding=e,
          ~encode=() => M.encode_response(resp),
          ~size=M.size_response,
          ~decode=M.decode_response,
        );
      },
      active_encodings(),
    );
  history :=
    List.map(
      (r: record) =>
        r.id == id
          ? {
            ...r,
            response,
          }
          : r,
      history^,
    );
};
