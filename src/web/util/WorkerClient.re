open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
let ackTimeoutDuration = 1000; // Worker attention timeout in ms
let evalTimeoutDuration = 20000; // Evaluation timeout in ms

type callbacks = {
  handler: Response.t => unit,
  timeout: Request.t => unit,
  on_ack: unit => unit,
};

type latest = {
  request_id: int,
  batch: Request.t,
  callbacks,
  /* Correlates this request with its WorkerMetrics record (None when
   * metrics are off) so the response-side benchmark lands on the row
   * the request-side benchmark created. */
  metrics_id: option(int),
};

let nextRequestId = ref(0);
let latestRequest: ref(option(latest)) = ref(None);
let ackTimeoutId = ref(None);
let evalTimeoutId = ref(None);

let clear_timer = timer_ref => {
  switch (timer_ref^) {
  | Some(id) => Dom_html.window##clearTimeout(id)
  | None => ()
  };
  timer_ref := None;
};

let clear_timeouts = () => {
  clear_timer(ackTimeoutId);
  clear_timer(evalTimeoutId);
};

let is_latest = request_id =>
  switch (latestRequest^) {
  | Some({request_id: latest_request_id, _}) =>
    request_id == latest_request_id
  | None => false
  };

/* Both directions cross postMessage in the Active encoding, not as live
 * values, to dodge the structured-clone overflow on deep results (#2368;
 * see WorkerServer.Active). Callers still deal in Request.t/Response.t. */
let post_evaluate = (worker, request_id, batch) =>
  worker##postMessage(
    Active.encode_request(
      ClientMessage.Evaluate({
        request_id,
        batch,
      }),
    ),
  );

let start_eval_timeout = latest => {
  clear_timer(evalTimeoutId);
  evalTimeoutId :=
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() =>
          if (is_latest(latest.request_id)) {
            clear_timeouts();
            latestRequest := None;
            latest.callbacks.timeout(latest.batch);
          }
        ),
        float_of_int(evalTimeoutDuration),
      ),
    );
};

let handle_ack = request_id =>
  if (is_latest(request_id)) {
    clear_timer(ackTimeoutId);
    switch (latestRequest^) {
    | Some(latest) =>
      latest.callbacks.on_ack();
      start_eval_timeout(latest);
    | None => ()
    };
  }
and handle_result = (request_id, response) =>
  if (is_latest(request_id)) {
    clear_timeouts();
    switch (latestRequest^) {
    | Some(latest) =>
      latestRequest := None;
      latest.callbacks.handler(response);
    | None => ()
    };
  };

let setupWorkerMessageHandler = worker => {
  worker##.onmessage :=
    Dom.handler(evt => {
      switch (Active.decode_response(evt##.data)) {
      | ServerMessage.Ack(request_id) => handle_ack(request_id)
      | ServerMessage.Result({request_id, response}) as msg =>
        /* Grab the metrics correlation id before handle_result clears
         * latestRequest. Hand the result off first; benchmarking the other
         * encodings can take tens of ms and must not delay evaluation
         * latency. */
        let metrics_id =
          switch (latestRequest^) {
          | Some({request_id: rid, metrics_id, _}) when rid == request_id =>
            metrics_id
          | _ => None
          };
        handle_result(request_id, response);
        switch (metrics_id) {
        | Some(id) => WorkerMetrics.record_response(id, msg)
        | None => ()
        };
      };
      Js._true;
    });
};

let initWorker: unit => Js.t(Worker.worker(Active.request, Active.response)) =
  () => {
    let worker = Worker.create(name);
    setupWorkerMessageHandler(worker);
    worker;
  };

let workerRef = ref(initWorker());

let restart_worker = (): unit => {
  workerRef.contents##terminate;
  workerRef.contents = initWorker();
};

let rec start_ack_timeout = latest => {
  clear_timer(ackTimeoutId);
  ackTimeoutId :=
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() =>
          if (is_latest(latest.request_id)) {
            restart_worker();
            post_evaluate(
              workerRef.contents,
              latest.request_id,
              latest.batch,
            );
            start_ack_timeout(latest);
          }
        ),
        float_of_int(ackTimeoutDuration),
      ),
    );
};

let request =
    (
      req: Request.t,
      ~handler: Response.t => unit,
      ~timeout: Request.t => unit,
      ~on_ack: unit => unit,
    )
    : unit =>
  switch (req) {
  | [] => ()
  | _ =>
    clear_timeouts();
    nextRequestId := nextRequestId^ + 1;
    /* When metrics are on, tag this request so the response can be
     * correlated, and benchmark the request-side encodings before posting. */
    let metrics_id =
      if (WorkerMetrics.enabled^) {
        let id = WorkerMetrics.next_id();
        WorkerMetrics.record_request(
          id,
          ClientMessage.Evaluate({
            request_id: nextRequestId^,
            batch: req,
          }),
        );
        Some(id);
      } else {
        None;
      };
    let latest = {
      request_id: nextRequestId^,
      batch: req,
      callbacks: {
        handler,
        timeout,
        on_ack,
      },
      metrics_id,
    };
    latestRequest := Some(latest);
    post_evaluate(workerRef.contents, latest.request_id, latest.batch);
    start_ack_timeout(latest);
  };
