open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
/* Warm-worker attention timeout. Must exceed a typical eval slice so a busy
 * but healthy worker is not killed mid-slice before it can ACK. */
let ackTimeoutDuration = 5000;
/* After terminate+respawn the browser must fetch/parse worker.js (multi-MB). */
let ackColdStartTimeoutDuration = 15000;
let maxAckRetries = 3;
let evalTimeoutDuration = 20000; // Evaluation timeout in ms

type callbacks = {
  handler: Response.t => unit,
  timeout: Request.batch => unit,
  on_ack:
    list((key, Language.IncrEval.t(Language.EvaluatorState.t))) => unit,
  on_stream:
    (key, Language.IncrEval.outbox(Language.EvaluatorState.t)) => unit,
};

type latest = {
  request: Request.t,
  callbacks,
  ack_retries: int,
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
  | Some({request: {request_id: latest_request_id, _}, _}) =>
    request_id == latest_request_id
  | None => false
  };

/* Both directions cross postMessage in the Active encoding, not as live
 * values, to dodge the structured-clone overflow on deep results (#2368;
 * see WorkerServer.Active). Callers still deal in Request.t/Response.t. */
let post_evaluate = (worker, request: Request.t) =>
  worker##postMessage(Active.encode_request(ClientMessage.Evaluate(request)));

let fail_latest = latest => {
  clear_timeouts();
  latestRequest := None;
  latest.callbacks.timeout(latest.request.batch);
};

/* Drop the in-flight request without invoking timeout/result callbacks.
 * Used when navigating away (slide/exercise switch) so stale stream chunks
 * cannot land on the newly selected editor. The worker may still finish the
 * abandoned request; client-side is_latest ignores its messages. A subsequent
 * request() posts Evaluate and the server abandons stale slices. */
let cancel = (): unit => {
  clear_timeouts();
  latestRequest := None;
};

let setupWorkerMessageHandler = worker => {
  worker##.onmessage :=
    Dom.handler(evt => {
      switch (Active.decode_response(evt##.data)) {
      | ServerMessage.Ack({request_id}) =>
        /* Liveness only — reuse tinting arrives next via ReusePlan. */
        if (is_latest(request_id)) {
          clear_timer(ackTimeoutId);
        }
      | ServerMessage.ReusePlan({request_id, initial}) =>
        if (is_latest(request_id)) {
          switch (latestRequest^) {
          | Some(latest) => latest.callbacks.on_ack(initial)
          | None => ()
          };
        }
      | ServerMessage.Stream(stream) =>
        if (is_latest(stream.request_id)) {
          switch (latestRequest^) {
          | Some(latest) =>
            latest.callbacks.on_stream(stream.key, stream.update)
          | None => ()
          };
        }
      | ServerMessage.Result({request_id, response}) as msg =>
        if (is_latest(request_id)) {
          /* Grab the metrics correlation id before clearing latestRequest.
           * Hand the result off first; benchmarking the other encodings can
           * take tens of ms and must not delay evaluation latency. */
          switch (latestRequest^) {
          | Some(latest) =>
            latestRequest := None;
            clear_timeouts();
            latest.callbacks.handler(response);
            switch (latest.metrics_id) {
            | Some(id) => WorkerMetrics.record_response(id, msg)
            | None => ()
            };
          | None => ()
          };
        }
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

/* Wall-clock cap for the whole request (including ACK wait). On expiry the
 * worker is terminated so a runaway eval cannot keep a core busy after the
 * UI has already shown Timeout. */
let start_eval_timeout = latest => {
  clear_timer(evalTimeoutId);
  evalTimeoutId :=
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() =>
          if (is_latest(latest.request.request_id)) {
            restart_worker();
            fail_latest(latest);
          }
        ),
        float_of_int(evalTimeoutDuration),
      ),
    );
};

let rec start_ack_timeout = (~cold_start, latest) => {
  clear_timer(ackTimeoutId);
  let duration = cold_start ? ackColdStartTimeoutDuration : ackTimeoutDuration;
  ackTimeoutId :=
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() =>
          if (is_latest(latest.request.request_id)) {
            if (latest.ack_retries >= maxAckRetries) {
              restart_worker();
              fail_latest(latest);
            } else {
              let latest = {
                ...latest,
                ack_retries: latest.ack_retries + 1,
              };
              latestRequest := Some(latest);
              restart_worker();
              post_evaluate(workerRef.contents, latest.request);
              start_ack_timeout(~cold_start=true, latest);
            };
          }
        ),
        float_of_int(duration),
      ),
    );
};

let request =
    (
      batch: Request.batch,
      ~handler: Response.t => unit,
      ~timeout: Request.batch => unit,
      ~on_ack:
         list((key, Language.IncrEval.t(Language.EvaluatorState.t))) => unit,
      ~on_stream:
         (key, Language.IncrEval.outbox(Language.EvaluatorState.t)) => unit,
    )
    : unit =>
  switch (batch) {
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
            batch,
          }),
        );
        Some(id);
      } else {
        None;
      };
    let latest = {
      request: {
        request_id: nextRequestId^,
        batch,
      },
      callbacks: {
        handler,
        timeout,
        on_ack,
        on_stream,
      },
      ack_retries: 0,
      metrics_id,
    };
    latestRequest := Some(latest);
    post_evaluate(workerRef.contents, latest.request);
    start_eval_timeout(latest);
    start_ack_timeout(~cold_start=false, latest);
  };
