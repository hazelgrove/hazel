open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
let timeoutDuration = 20000; // Worker timeout in ms

/* Worker exchanges Active-encoding payloads, not live values, to dodge the
 * structured-clone overflow on deep results (#2368; see WorkerServer.Active).
 * Callers still deal in Request.t/Response.t. */
let initWorker: unit => Js.t(Worker.worker(Active.request, Active.response)) =
  () => Worker.create(name);

let workerRef: ref(Js.t(Worker.worker(Active.request, Active.response))) =
  ref(initWorker());

let timeoutId = ref(None);

let restart_worker = (): unit => {
  workerRef.contents##terminate;
  workerRef.contents = initWorker();
};

let request =
    (
      req: Request.t,
      ~handler: Response.t => unit,
      ~timeout: Request.t => unit,
    )
    : unit => {
  /* When metrics are on, tag this request so the response can be correlated,
   * and benchmark the request-side encodings before posting. */
  let metrics_id =
    if (WorkerMetrics.enabled^) {
      let id = WorkerMetrics.next_id();
      WorkerMetrics.record_request(id, req);
      Some(id);
    } else {
      None;
    };
  let setupWorkerMessageHandler = worker => {
    worker##.onmessage :=
      Dom.handler(evt => {
        switch (timeoutId.contents) {
        | Some(id) => Dom_html.window##clearTimeout(id)
        | None => ()
        };
        timeoutId.contents = None; /* Clear timeout after response */
        let resp = Active.decode_response(evt##.data);
        /* Hand the result off first; benchmarking the other variants can take
         * tens of ms and must not delay evaluation latency. */
        handler(resp);
        switch (metrics_id) {
        | Some(id) => WorkerMetrics.record_response(id, resp)
        | None => ()
        };
        Js._true;
      });
  };

  /* If there's an ongoing request, terminate the worker and reinitialize */
  switch (timeoutId.contents) {
  | Some(id) =>
    Dom_html.window##clearTimeout(id);
    restart_worker();
  | None => ()
  };

  setupWorkerMessageHandler(workerRef.contents);

  workerRef.contents##postMessage(Active.encode_request(req));

  let onTimeout = (): unit => {
    restart_worker();
    setupWorkerMessageHandler(workerRef.contents);
    timeout(req);
  };

  timeoutId.contents =
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(onTimeout),
        float_of_int(timeoutDuration),
      ),
    );
};

let request = (req, ~handler, ~timeout) =>
  switch (req) {
  | [] => ()
  | _ => request(req, ~handler, ~timeout)
  };
