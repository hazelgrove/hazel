open Js_of_ocaml;

/* Data for the "Worker Messaging" debug panel: how the main thread talks to
 * the eval Web Worker. Per-request benchmarking of the candidate encodings
 * (WorkerServer.encoding / ENCODING) that pack payloads across the boundary.
 *
 * For every enabled encoding we encode the real payload, run it through the
 * browser's structuredClone (the same serializer postMessage uses, so an
 * encoding that overflows the clone stack — e.g. Direct on a deep result,
 * #2368 — surfaces here as a caught exception), then decode, timing each stage
 * and approximating the encoded size. Results feed the Worker Messaging table
 * in DebugSidebar.
 *
 * Everything runs on the main thread; nothing crosses to the worker. Gated by
 * `enabled` (synced from show_debug_panel in Page.Update.calculate) so normal
 * editing pays nothing. */

let enabled = ref(false);

/* Encodings the user has turned on in the panel (WorkerServer.encoding);
   synced from settings in Page.Update.calculate. Only enabled encodings are
   measured, so e.g. the slow sexp encoding can be skipped. */
let enabled_encodings: ref(list(WorkerServer.encoding)) = ref([]);

let active_encodings = (): list(WorkerServer.encoding) =>
  List.filter(
    e => List.mem(e, enabled_encodings^),
    WorkerServer.all_of_encoding,
  );

type status =
  | Ok
  | Failed(string);

/* One encoding measured in one direction. */
type dir_metric = {
  encoding: WorkerServer.encoding,
  encode_ms: float,
  clone_ms: float,
  decode_ms: float,
  size_bytes: int,
  status,
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

/* Approximate in-memory footprint of an arbitrary wire form (string, OCaml
 * value graph, plain object, or typed arrays), by an iterative walk with a
 * visited set so shared/DAG structure isn't double-counted. Numbers 8B,
 * strings by length, typed arrays by byteLength, object/array headers ~8B per
 * slot. Only a relative measure across variants, not an exact byte count. */
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
let size_bytes: 'a. 'a => int =
  x => Js.Unsafe.fun_call(size_fn, [|Js.Unsafe.inject(x)|]);

let timed: 'a. (unit => 'a) => (float, 'a) =
  f => {
    let t0 = Util.JsUtil.precise_timestamp();
    let x = f();
    (Util.JsUtil.precise_timestamp() -. t0, x);
  };

/* Measure encode -> structuredClone -> decode for one wire direction. Each
 * stage that runs before an exception keeps its timing; the first stage to
 * throw sets status to Failed and stops (later stages stay 0). */
let measure:
  'w 'a.
  (~encoding: WorkerServer.encoding, ~encode: unit => 'w, ~decode: 'w => 'a) =>
  dir_metric
 =
  (~encoding, ~encode, ~decode) => {
    let enc = ref(0.)
    and cln = ref(0.)
    and dec = ref(0.)
    and sz = ref(0);
    let status =
      switch (
        {
          let (e, encoded) = timed(encode);
          enc := e;
          let (c, cloned) = timed(() => structured_clone(encoded));
          cln := c;
          sz := size_bytes(cloned);
          let (d, _) = timed(() => decode(cloned));
          dec := d;
        }
      ) {
      | () => Ok
      | exception exn => Failed(Printexc.to_string(exn))
      };
    {
      encoding,
      encode_ms: enc^,
      clone_ms: cln^,
      decode_ms: dec^,
      size_bytes: sz^,
      status,
    };
  };

let push = (r: record): unit =>
  history := [r, ...Util.ListUtil.take(history_limit - 1, history^)];

let record_request = (id: int, req: WorkerServer.Request.t): unit => {
  let request =
    List.map(
      (e: WorkerServer.encoding) => {
        module M = (val WorkerServer.module_of_encoding(e));
        measure(
          ~encoding=e,
          ~encode=() => M.encode_request(req),
          ~decode=M.decode_request,
        );
      },
      active_encodings(),
    );
  push({
    id,
    entries: List.length(req),
    request,
    response: [],
  });
};

let record_response = (id: int, resp: WorkerServer.Response.t): unit => {
  let response =
    List.map(
      (e: WorkerServer.encoding) => {
        module M = (val WorkerServer.module_of_encoding(e));
        measure(
          ~encoding=e,
          ~encode=() => M.encode_response(resp),
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
