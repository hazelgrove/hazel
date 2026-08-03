open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
/* Warm-worker ack timeout. Must exceed a typical eval slice so a busy
 * but healthy worker is not killed mid-slice before it can ACK. */
let ack_timeout_ms = 5000;
/* After terminate+respawn the browser must fetch/parse worker.js (multi-MB). */
let ack_cold_start_timeout_ms = 15000;
let max_ack_retries = 3;
let eval_timeout_ms = 20000; // Evaluation timeout in ms

type callbacks = {
  on_result: Response.t => unit,
  on_timeout: Request.batch => unit,
  on_ack: ServerMessage.reuse_predictions => unit,
  on_stream: (key, ServerMessage.stream_update) => unit,
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

let next_request_id = ref(0);
let latest_request: ref(option(latest)) = ref(None);
let ack_timeout_id = ref(None);
let eval_timeout_id = ref(None);

let clear_timer = timer_ref => {
  switch (timer_ref^) {
  | Some(id) => Dom_html.window##clearTimeout(id)
  | None => ()
  };
  timer_ref := None;
};

let clear_timeouts = () => {
  clear_timer(ack_timeout_id);
  clear_timer(eval_timeout_id);
};

/* Run f on the current request iff request_id still matches it; messages
 * and timers for superseded requests are dropped. */
let with_latest = (request_id, f) =>
  switch (latest_request^) {
  | Some({request: {request_id: latest_id, _}, _} as latest)
      when request_id == latest_id =>
    f(latest)
  | Some(_)
  | None => ()
  };

/* Both directions cross postMessage in the Active encoding, not as live
 * values, to dodge the structured-clone overflow on deep results (#2368;
 * see WorkerServer.Active). Callers still deal in Request.t/Response.t. */
let post_evaluate = (worker, request: Request.t) =>
  worker##postMessage(
    Active.encode_request(ClientMessage.Evaluate(request)),
  );

let fail_latest = latest => {
  clear_timeouts();
  latest_request := None;
  latest.callbacks.on_timeout(latest.request.batch);
};

/* Drop the in-flight request without invoking timeout/result callbacks.
 * Used when navigating away (slide/exercise switch) so stale stream chunks
 * cannot land on the newly selected editor. The worker may still finish the
 * abandoned request; client-side with_latest ignores its messages. A
 * subsequent request() posts Evaluate and the server abandons stale slices. */
let cancel = (): unit => {
  clear_timeouts();
  latest_request := None;
};

let setup_worker_message_handler = worker => {
  worker##.onmessage :=
    Dom.handler(evt => {
      switch (Active.decode_response(evt##.data)) {
      | ServerMessage.Ack({request_id}) =>
        /* Liveness only — reuse tinting arrives next via ReusePlan. */
        with_latest(request_id, _ => clear_timer(ack_timeout_id))
      | ServerMessage.ReusePlan({request_id, initial}) =>
        with_latest(request_id, latest => latest.callbacks.on_ack(initial))
      | ServerMessage.Stream({request_id, key, update}) =>
        with_latest(request_id, latest =>
          latest.callbacks.on_stream(key, update)
        )
      | ServerMessage.Result({request_id, response}) as msg =>
        with_latest(
          request_id,
          latest => {
            clear_timeouts();
            latest_request := None;
            /* Hand the result off first; benchmarking the other encodings
             * can take tens of ms and must not delay evaluation latency. */
            latest.callbacks.on_result(response);
            switch (latest.metrics_id) {
            | Some(id) => WorkerMetrics.record_response(id, msg)
            | None => ()
            };
          },
        )
      };
      Js._true;
    });
};

let init_worker: unit => Js.t(Worker.worker(Active.request, Active.response)) =
  () => {
    let worker = Worker.create(name);
    setup_worker_message_handler(worker);
    worker;
  };

let worker_ref = ref(init_worker());

let restart_worker = (): unit => {
  worker_ref.contents##terminate;
  worker_ref.contents = init_worker();
};

/* Wall-clock cap for the whole request (including ACK wait). On expiry the
 * worker is terminated so a runaway eval cannot keep a core busy after the
 * UI has already shown Timeout. */
let start_eval_timeout = latest => {
  clear_timer(eval_timeout_id);
  eval_timeout_id :=
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() =>
          with_latest(
            latest.request.request_id,
            latest => {
              restart_worker();
              fail_latest(latest);
            },
          )
        ),
        float_of_int(eval_timeout_ms),
      ),
    );
};

let rec start_ack_timeout = (~cold_start, latest) => {
  clear_timer(ack_timeout_id);
  let duration = cold_start ? ack_cold_start_timeout_ms : ack_timeout_ms;
  ack_timeout_id :=
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() =>
          with_latest(latest.request.request_id, latest =>
            if (latest.ack_retries >= max_ack_retries) {
              restart_worker();
              fail_latest(latest);
            } else {
              let latest = {
                ...latest,
                ack_retries: latest.ack_retries + 1,
              };
              latest_request := Some(latest);
              restart_worker();
              post_evaluate(worker_ref.contents, latest.request);
              start_ack_timeout(~cold_start=true, latest);
            }
          )
        ),
        float_of_int(duration),
      ),
    );
};

let request =
    (
      batch: Request.batch,
      ~on_result: Response.t => unit,
      ~on_timeout: Request.batch => unit,
      ~on_ack: ServerMessage.reuse_predictions => unit,
      ~on_stream: (key, ServerMessage.stream_update) => unit,
    )
    : unit =>
  switch (batch) {
  | [] => ()
  | _ =>
    clear_timeouts();
    next_request_id := next_request_id^ + 1;
    /* When metrics are on, tag this request so the response can be
     * correlated, and benchmark the request-side encodings before posting. */
    let metrics_id =
      if (WorkerMetrics.enabled^) {
        let id = WorkerMetrics.next_id();
        WorkerMetrics.record_request(
          id,
          ClientMessage.Evaluate({
            request_id: next_request_id^,
            batch,
          }),
        );
        Some(id);
      } else {
        None;
      };
    let latest = {
      request: {
        request_id: next_request_id^,
        batch,
      },
      callbacks: {
        on_result,
        on_timeout,
        on_ack,
        on_stream,
      },
      ack_retries: 0,
      metrics_id,
    };
    latest_request := Some(latest);
    post_evaluate(worker_ref.contents, latest.request);
    start_eval_timeout(latest);
    start_ack_timeout(~cold_start=false, latest);
  };
