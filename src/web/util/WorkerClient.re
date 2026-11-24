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
  /* posted with every piece, after the worker asked */
  resent: bool,
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

/* the pieces the worker holds: those of the last request posted to it,
   as it keeps exactly those. a new worker holds none */
let held: Stdlib.Hashtbl.t(EvalPieces.hash, unit) =
  Stdlib.Hashtbl.create(256);

/* what the worker holds once it receives [request] */
let hold = (request: Request.t): unit => {
  Stdlib.Hashtbl.reset(held);
  List.iter(
    ~f=
    ((_, v: Request.value)) =>
      List.iter(
          ~f=((hash, _)) => Stdlib.Hashtbl.replace(held, hash, ()),
        v.program.pieces,
      ),
    request.batch,
  );
};

/* [request] as posted: its programs' layouts, and the pieces the worker
   doesn't hold ([full]: all of them) */
let posted_of = (~full: bool, request: Request.t): Posted.t => {
  let seen = Stdlib.Hashtbl.create(64);
  {
    request_id: request.request_id,
    batch:
      List.map(
        ~f=
        ((key, v: Request.value)) =>
          (
            key,
            Posted.{
              layout: v.program.layout,
              prev: v.prev,
              stream: v.stream,
            },
          ),
        request.batch,
      ),
    pieces:
      List.concat_map(
        ~f=
        ((_, v: Request.value)) =>
          List.filter(
              ~f=
            ((hash, _)) =>
                  (full || !Stdlib.Hashtbl.mem(held, hash))
                  && !Stdlib.Hashtbl.mem(seen, hash)
              && {
                    Stdlib.Hashtbl.replace(seen, hash, ());
                true;
              },
            v.program.pieces,
          ),
        request.batch,
      ),
  };
};

/* Both directions cross postMessage in the Active encoding, not as live
 * values, to dodge the structured-clone overflow on deep results (#2368;
 * see WorkerServer.Active). Callers still deal in Request.t/Response.t.
 *
 * Encodes once: the Evaluation panel needs the posted byte length, and records
 * the send here so its latency clock starts as close to the post as it can.
 * Benchmarking the request-side encodings ([metrics]) happens before, keeping
 * that (heavy) work outside the latency clock. */
let post_evaluate = (~full=false, ~metrics=false, worker, request: Request.t) => {
  let msg = ClientMessage.Evaluate(posted_of(~full, request));
  if (metrics) {
    WorkerMetrics.record_request(request.request_id, msg);
  };
  let encoded = Active.encode_request(msg);
  EvalMetrics.record_sent(~request, ~encoded);
  worker##postMessage(encoded);
  hold(request);
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

/* the worker asked for every piece of the latest request: set below,
   where the ack timeout is (the handler is installed before it) */
let resend: ref(latest => unit) = ref(_ => ());

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
      | ServerMessage.Resend({request_id}) =>
        with_latest(request_id, latest => resend^(latest))
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

/* the standard library the app installed after an edit (a Marshal of
   Library.baked); a worker starts with the shipped one, so a new or
   restarted worker gets this first */
let library: ref(option(string)) = ref(None);

let post_library = (worker, blob: string) =>
  worker##postMessage(
    Active.encode_request(ClientMessage.InstallLibrary(blob)),
  );

let init_worker: unit => Js.t(Worker.worker(Active.request, Active.response)) =
  () => {
    let worker = Worker.create(name);
    setup_worker_message_handler(worker);
    Option.iter(post_library(worker), library^);
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

/* posted before any later evaluation request (postMessage is FIFO) */
let install_library = (blob: string): unit => {
  library := Some(blob);
  Option.iter(w => post_library(w, blob), worker_ref.contents);
};

let restart_worker = (): unit => {
  EvalMetrics.incr_restarts();
  switch (worker_ref.contents) {
  | Some(w) => w##terminate
  | None => ()
  };
  Stdlib.Hashtbl.reset(held);
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

/* asked again after posting every piece: fail it rather than loop */
resend :=
  (
    latest =>
      if (latest.resent) {
        print_endline("eval worker: pieces missing from a full request");
        fail_latest(latest);
      } else {
        clear_timer(ack_timeout_id);
        let latest = {
          ...latest,
          resent: true,
        };
        latest_request := Some(latest);
        post_evaluate(~full=true, get_worker(), latest.request);
        start_ack_timeout(~cold_start=false, latest);
      }
  );

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
    let request: Request.t = {
      request_id: next_request_id^,
      batch,
    };
    let latest = {
      request,
      callbacks: {
        on_result,
        on_timeout,
        on_ack,
        on_stream,
      },
      ack_retries: 0,
      resent: false,
    };
    latest_request := Some(latest);
    /* The request id doubles as the metrics row id, so the two panels
     * correlate. */
    post_evaluate(~metrics=true, get_worker(), latest.request);
    start_eval_timeout(latest);
    start_ack_timeout(~cold_start=false, latest);
  };
