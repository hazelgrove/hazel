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
};

/* The live worker's latest coverage, and the last from each worker
 * restart_worker terminated. See CoverageHook. */
let live_coverage: ref(option(string)) = ref(None);
let retired_coverage: ref(list(string)) = ref([]);

let worker_coverage = (): list(string) =>
  Option.to_list(live_coverage^) @ retired_coverage^;

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

/* A deadline measured on a main-thread timer is only fair if the page could
   hear the worker meanwhile. When the main thread is blocked -- statics on a
   large slide held Firefox's for 22-26 s at a stretch -- the timer fires late,
   and the worker's reply, sent long before, can be queued BEHIND it: the
   timeout ran first, killed a healthy worker that had answered in 30 ms,
   respawned it, and the respawn's own blocked stretch did it again, so the
   slide never drew ("Evaluation timed out").

   So a timer that fires more than [late_slack_ms] late does not decide. It
   waits [grace_ms] more, long enough for anything already queued to be
   handled first, and asks again. A grace timer that is itself late may wait
   again, but at most [max_postponements] times per deadline: a hidden tab's
   timers are throttled to about once a second, so every grace timer there
   fires late, and without a cap a runaway loop in a background tab would
   never be stopped (until the worker's own ~200 s step limit) and a dead
   worker never reported. The cap bounds that to a few seconds past the
   deadline (about 3 s with timers throttled to 1 s), while a busy but
   visible page still hears any queued reply first. */
let late_slack_ms = 250.;
let grace_ms = 500;
let max_postponements = 2;

type deadline_check =
  | Expire
  | Postpone;

/* What a deadline timer does when it fires [late_ms] after it was due,
   having already been postponed [postponed] times. */
let check_deadline = (~late_ms: float, ~postponed: int): deadline_check =>
  Float.(late_ms > late_slack_ms) && postponed < max_postponements
    ? Postpone : Expire;

let set_deadline = (timer_ref, ~delay_ms: int, on_expire: unit => unit) => {
  let rec arm = (~delay_ms, ~postponed) => {
    let due = Util.JsUtil.precise_timestamp() +. float_of_int(delay_ms);
    timer_ref :=
      Some(
        Dom_html.window##setTimeout(
          Js.wrap_callback(() =>
            switch (
              check_deadline(
                ~late_ms=Util.JsUtil.precise_timestamp() -. due,
                ~postponed,
              )
            ) {
            | Postpone => arm(~delay_ms=grace_ms, ~postponed=postponed + 1)
            | Expire => on_expire()
            }
          ),
          float_of_int(delay_ms),
        ),
      );
  };
  arm(~delay_ms, ~postponed=0);
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
 * see WorkerServer.Active). Callers still deal in Request.t/Response.t.
 *
 * Encodes once: the Evaluation panel needs the posted byte length, and records
 * the send here so its latency clock starts as close to the post as it can. */
let post_evaluate = (worker, request: Request.t) => {
  let encoded = Active.encode_request(ClientMessage.Evaluate(request));
  EvalMetrics.record_sent(~request, ~encoded);
  worker##postMessage(encoded);
};

let fail_latest = latest => {
  clear_timeouts();
  latest_request := None;
  EvalMetrics.record_timeout(~id=latest.request.request_id);
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
      /* Stop the latency clock first thing, before any decode/benchmark. */
      let now = Util.JsUtil.precise_timestamp();
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
      | ServerMessage.Result(result) as msg =>
        with_latest(
          result.request_id,
          latest => {
            clear_timeouts();
            latest_request := None;
            /* Hand the result off first; benchmarking the other encodings
             * can take tens of ms and must not delay evaluation latency. */
            latest.callbacks.on_result(result.response);
            WorkerMetrics.record_response(result.request_id, msg);
            EvalMetrics.record_done(~now, ~encoded=evt##.data, result);
          },
        )
      | ServerMessage.Coverage(coverage) => live_coverage := Some(coverage)
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

/* Main starts the worker at page load (see there); get_worker also creates
   it on first use if nothing has yet. Not at module init: the test binary
   links this module (via ScratchMode) under node, where Worker doesn't
   exist. */
let worker_ref:
  ref(option(Js.t(Worker.worker(Active.request, Active.response)))) =
  ref(None);

let get_worker = () =>
  switch (worker_ref.contents) {
  | Some(w) => w
  | None =>
    let w = init_worker();
    worker_ref.contents = Some(w);
    w;
  };

let restart_worker = (): unit => {
  EvalMetrics.incr_restarts();
  switch (worker_ref.contents) {
  | Some(w) => w##terminate
  | None => ()
  };
  retired_coverage := worker_coverage();
  live_coverage := None;
  worker_ref.contents = Some(init_worker());
};

/* Wall-clock cap for the whole request (including ACK wait). On expiry the
 * worker is terminated so a runaway eval cannot keep a core busy after the
 * UI has already shown Timeout. */
let start_eval_timeout = latest => {
  clear_timer(eval_timeout_id);
  set_deadline(eval_timeout_id, ~delay_ms=eval_timeout_ms, () =>
    with_latest(
      latest.request.request_id,
      latest => {
        restart_worker();
        fail_latest(latest);
      },
    )
  );
};

let rec start_ack_timeout = (~cold_start, latest) => {
  clear_timer(ack_timeout_id);
  let duration = cold_start ? ack_cold_start_timeout_ms : ack_timeout_ms;
  set_deadline(ack_timeout_id, ~delay_ms=duration, () =>
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
        post_evaluate(get_worker(), latest.request);
        start_ack_timeout(~cold_start=true, latest);
      }
    )
  );
};

/* A request supersedes the one in flight only for the keys it carries:
   the rest of the old batch rides along, its replies still going to the
   old callbacks. Otherwise a cell whose inputs have not changed since
   never hears back (Next onto a task slide: the slide's Auto Probe
   default re-requests YourImpl alone, and HiddenTests stayed empty). */
let supersede =
    (prev: option(latest), batch: Request.batch, callbacks: callbacks)
    : (Request.batch, callbacks) => {
  let carried =
    switch (prev) {
    | Some(prev) =>
      List.filter(prev.request.batch, ~f=((key, _)) =>
        !List.Assoc.mem(batch, key, ~equal=String.equal)
      )
    | None => []
    };
  switch (prev) {
  | Some(prev) when !List.is_empty(carried) =>
    let is_old = key => List.Assoc.mem(carried, key, ~equal=String.equal);
    let split = (old_f, new_f, items) => {
      let (old, fresh) =
        List.partition_tf(items, ~f=((key, _)) => is_old(key));
      if (!List.is_empty(old)) {
        old_f(old);
      };
      if (!List.is_empty(fresh)) {
        new_f(fresh);
      };
    };
    (
      batch @ carried,
      {
        on_result: split(prev.callbacks.on_result, callbacks.on_result),
        on_timeout: split(prev.callbacks.on_timeout, callbacks.on_timeout),
        on_ack: split(prev.callbacks.on_ack, callbacks.on_ack),
        on_stream: (key, update) =>
          (is_old(key) ? prev.callbacks.on_stream : callbacks.on_stream)(
            key,
            update,
          ),
      },
    );
  | _ => (batch, callbacks)
  };
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
    let (batch, callbacks) =
      supersede(
        latest_request^,
        batch,
        {
          on_result,
          on_timeout,
          on_ack,
          on_stream,
        },
      );
    next_request_id := next_request_id^ + 1;
    let request: Request.t = {
      request_id: next_request_id^,
      batch,
    };
    /* The request id doubles as the metrics row id, so the two panels correlate.
     * Benchmarking the request-side encodings happens before the post, keeping
     * that (heavy) work outside the latency clock. */
    WorkerMetrics.record_request(
      request.request_id,
      ClientMessage.Evaluate(request),
    );
    let latest = {
      request,
      callbacks,
      ack_retries: 0,
    };
    latest_request := Some(latest);
    post_evaluate(get_worker(), latest.request);
    start_eval_timeout(latest);
    start_ack_timeout(~cold_start=false, latest);
  };
