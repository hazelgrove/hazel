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
  on_result:
    (~final_streams: list((key, ServerMessage.stream_update)), Response.t) =>
    unit,
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

let next_request_id = ref(0);
let latest_request: ref(option(latest)) = ref(None);
let ack_timeout_id = ref(None);

/* The request whose messages are still being taken, if any. */
let latest_request_id = (): option(int) =>
  Option.map(l => l.request.request_id, latest_request^);
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
   and the worker's reply, sent long before, is queued BEHIND it: the timeout
   ran first, killed a healthy worker that had answered in 30 ms, respawned
   it, and the respawn's own blocked stretch did it again, so the slide never
   drew ("Evaluation timed out").

   So a timer that fires more than [late_slack_ms] late does not decide. It
   waits [grace_ms] more, long enough for anything already queued to be
   handled first, and asks again; a grace timer that is itself late waits
   again. A worker that is really gone is still caught, as soon as the page
   has been free to hear it. */
let late_slack_ms = 250.;
let grace_ms = 500;
let rec set_deadline = (timer_ref, ~delay_ms: int, on_expire: unit => unit) => {
  let due = Util.JsUtil.precise_timestamp() +. float_of_int(delay_ms);
  timer_ref :=
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(() =>
          if (Util.JsUtil.precise_timestamp() -. due > late_slack_ms) {
            set_deadline(timer_ref, ~delay_ms=grace_ms, on_expire);
          } else {
            on_expire();
          }
        ),
        float_of_int(delay_ms),
      ),
    );
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
let held: Hashtbl.t(EvalPieces.hash, unit) = Hashtbl.create(256);

/* what the worker holds once it receives [request] */
let hold = (request: Request.t): unit => {
  Hashtbl.reset(held);
  List.iter(
    ((_, v: Request.value)) =>
      List.iter(
        ((hash, _)) => Hashtbl.replace(held, hash, ()),
        v.program.pieces,
      ),
    request.batch,
  );
};

/* a piece's bytes: [shared], the values the worker already has go as
   references (Util.ShareCodec), unless it is too deep to walk */
let bytes_of = (~shared: bool, piece: EvalPieces.piece): string =>
  switch (piece) {
  | Bytes(bytes) => bytes
  | Content(c) when shared =>
    EvalPieces.encode(
      switch (Util.ShareCodec.Client.encode(c)) {
      | c => c
      | exception _ => c
      },
    )
  | Content(c) => EvalPieces.encode(c)
  };

/* [request] as posted: its programs' layouts, and the pieces the worker
   doesn't hold ([full]: all of them), encoded in order ([shared]: each can
   refer to the values of those before it) */
let posted_of = (~full: bool, ~shared=false, request: Request.t): Posted.t => {
  let seen = Hashtbl.create(64);
  {
    request_id: request.request_id,
    batch:
      List.map(
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
        ((_, v: Request.value)) =>
          List.filter(
            ((hash, _)) =>
              (full || !Hashtbl.mem(held, hash))
              && !Hashtbl.mem(seen, hash)
              && {
                Hashtbl.replace(seen, hash, ());
                true;
              },
            v.program.pieces,
          ),
        request.batch,
      )
      /* (List.map applies in order) */
      |> List.map(((hash, piece)) => (hash, bytes_of(~shared, piece))),
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
/* (^ticket(513)) The pieces a request posts go shared (Util.ShareCodec):
   a changed definition is a new piece, and a livelit release changes one,
   which in Kids' Choice was ~490 KB posted per release, against ~16 KB
   when the whole request was shared (before #2469's pieces), and the
   first post came ~70 ms later. Shared, its values the worker already has
   go as references. Whether the worker's table must start over with the
   next request: at first, after the worker restarts, for a request posted
   in full (a Resend: a piece it could not resolve), or when the client's
   table is full. */
let share_reset = ref(true);

let post_evaluate = (~full=false, ~metrics=false, worker, request: Request.t) => {
  if (full || Util.ShareCodec.Client.size() > Util.ShareCodec.max_entries) {
    share_reset := true;
  };
  let reset = share_reset^;
  if (reset) {
    Util.ShareCodec.Client.reset();
  };
  share_reset := false;
  let msg =
    ClientMessage.EvaluateShared(
      reset,
      posted_of(~full, ~shared=true, request),
    );
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

/* Shared responses (WorkerServer.post_message): after one named a value
   this client does not hold, the rest of the old stream is ignored until
   the worker starts its table over. */
let awaiting_response_reset = ref(false);

/* A response as the worker meant it, a shared one decoded. None: ignored,
   while waiting for the worker's reset. Error: it named a value this
   client does not hold; the caller asks for the reset (and the
   evaluation again, whose answer it was). */
let unshare = (msg: ServerMessage.t): option(result(ServerMessage.t, unit)) =>
  switch (msg) {
  | ServerMessage.Shared(reset, shared) =>
    if (reset) {
      Util.ShareCodec.Worker.reset();
      awaiting_response_reset := false;
    };
    if (awaiting_response_reset^) {
      None;
    } else {
      switch (Util.ShareCodec.Worker.decode(shared)) {
      | msg => Some(Ok(msg))
      | exception _ =>
        Util.ShareCodec.Worker.reset();
        awaiting_response_reset := true;
        Some(Error());
      };
    };
  | msg => Some(Ok(msg))
  };

/* the worker asked for every piece of the latest request: set below,
   where the ack timeout is (the handler is installed before it) */
let resend: ref(latest => unit) = ref(_ => ());

let setup_worker_message_handler = worker => {
  worker##.onmessage :=
    Dom.handler(evt => {
      /* Stop the latency clock first thing, before any decode/benchmark. */
      let now = Util.JsUtil.precise_timestamp();
      let handle = (msg: ServerMessage.t) =>
        switch (msg) {
        | ServerMessage.Shared(_) => () /* unshared before here */
        | ServerMessage.Resend({request_id}) =>
          with_latest(request_id, latest => resend^(latest))
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
              latest.callbacks.on_result(
                ~final_streams=
                  List.map(
                    (st: ServerMessage.stream) => (st.key, st.update),
                    result.final_streams,
                  ),
                result.response,
              );
              WorkerMetrics.record_response(result.request_id, msg);
              EvalMetrics.record_done(~now, ~encoded=evt##.data, result);
            },
          )
        };
      switch (unshare(Active.decode_response(evt##.data))) {
      | Some(Ok(msg)) => handle(msg)
      | None => ()
      | Some(Error ()) =>
        worker##postMessage(
          Active.encode_request(ClientMessage.ResetResponses),
        );
        switch (latest_request^) {
        | Some(latest) => post_evaluate(worker, latest.request)
        | None => ()
        };
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

/* Created on first use, not at module init: the test binary links this
   module (via ScratchMode) under node, where Worker doesn't exist. */
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
  Hashtbl.reset(held);
  share_reset := true;
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
      ~on_result:
         (
           ~final_streams: list((key, ServerMessage.stream_update)),
           Response.t
         ) =>
         unit,
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
