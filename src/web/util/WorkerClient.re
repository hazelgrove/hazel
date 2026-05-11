open Js_of_ocaml;
open WorkerServer;

/* WorkerClient
 * ============
 *
 * Editor-side glue for the dynamic incremental evaluator. Three concerns:
 *
 *  1. Outbox: `request` does NOT postMessage. Each call writes the latest
 *     payload into an editor-side ref, and a debounced flush callback is
 *     the single place that posts to the worker. This means rapid keys
 *     (or a fast typing session) collapse into one postMessage per
 *     debounce window — keys never restart the worker by themselves.
 *
 *  2. Generations: every postMessage is stamped with a monotonically
 *     increasing generation; the worker echoes the highest generation
 *     it observed in `Done.processed_gen`. On Done, if either the
 *     outbox is non-empty OR `last_sent_gen > processed_gen`, the
 *     editor knows an update was missed (e.g. landed in a worker that
 *     was about to self-close) and respawns + re-sends.
 *
 *  3. Worker lifecycle: the editor never calls `terminate`. The worker
 *     self-exits via self.close() after every Done. The next request
 *     lazily spawns a fresh worker. */

let name = "worker.js";

/* Debounce window for the outbox. Coalesces a typing burst into a single
 * postMessage; user-perceived latency stays at this scale. */
let flush_delay_ms = 50.0;

/* Optional per-handler hook exposed when results land. */
type response_handler = Response.t => unit;

/* Internal handler ref so that the latest editor-installed handler is
 * always called when Done arrives (handlers may close over freshly-bound
 * setters from a re-render, so we keep this updatable). */
let response_handler: ref(response_handler) = ref(_ => ());

let set_response_handler = (h: response_handler): unit =>
  response_handler := h;

/* Currently-attached worker, lazily created. None means "no worker
 * exists right now"; the next flush will spawn one. */
let worker_ref:
  ref(option(Js.t(Worker.worker(Request.t, Response.t)))) =
  ref(None);

/* Latest payload to send. New cells overwrite older ones with the same
 * key; older cells with no fresh entry are kept. This way, a typing
 * burst that only edits cell A doesn't drop cell B's pending update. */
let outbox: ref(list((string, Request.value))) = ref([]);

/* Monotonic generations: next_gen counts every successful flush.
 * last_sent_gen is what the most recent postMessage carried. */
let next_gen: ref(int) = ref(0);
let last_sent_gen: ref(int) = ref(-1);
let last_done_gen: ref(int) = ref(-1);

/* Flush timer id; None when no flush is scheduled. We use the method
 * form `window##setTimeout` (which returns Dom_html.timeout_id) rather
 * than the safe binding (which returns timeout_id_safe and would
 * mismatch clearTimeout's parameter type). */
let flush_timer: ref(option(Dom_html.timeout_id)) = ref(None);

/* Merge a new batch into the outbox: per-key, the new entry wins.
 * Entries the editor doesn't include in this call are preserved
 * (so cell B's pending update isn't dropped if only cell A is sent). */
let merge_outbox =
    (incoming: list((string, Request.value)))
    : list((string, Request.value)) => {
  let in_keys = List.map(fst, incoming);
  let kept =
    List.filter(((k, _)) => !List.mem(k, in_keys), outbox^);
  kept @ incoming;
};

/* Highest generation we've accepted a Progress for. Stale Progress
 * messages (older than the most recent Done, or older than the most
 * recent flush) are dropped — the editor should never roll back its
 * UI to an earlier run's partial state. */
let last_progress_gen: ref(int) = ref(-1);

/* Forward declaration: flush sets up the worker if needed and posts.
 * setup_worker_handlers wires both Progress (mid-flight, no
 * teardown) and Done (final, drop worker_ref + lost-update check)
 * through `response_handler`. */
let rec setup_worker_handlers =
        (worker: Js.t(Worker.worker(Request.t, Response.t))): unit => {
  worker##.onmessage :=
    Dom.handler(evt => {
      let resp: Response.t = evt##.data;
      let gen = Response.processed_gen(resp);
      switch (resp) {
      | Progress(_) =>
        /* Drop Progress messages whose generation has been superseded
         * — they describe a run the editor has logically replaced
         * (e.g. a debounced batch fired before the worker noticed
         * pending_update). Without this, a late Progress could shrink
         * a freshly-initialized pending_set or stamp stale dynamics
         * over a newer run's. We also drop Progress arriving after a
         * Done for the same generation: postMessage is FIFO from a
         * single worker, but a freshly-spawned worker for the next
         * generation could in principle still race with a leftover
         * Progress in flight from the old one. */
        if (gen >= last_sent_gen^
            && gen > last_progress_gen^
            && gen > last_done_gen^) {
          last_progress_gen := gen;
          response_handler^(resp);
        };
        ();
      | Done(_) =>
        last_done_gen := gen;
        /* Worker is going to self-close; drop our reference. The next
         * flush will spawn a fresh one. */
        worker_ref := None;
        response_handler^(resp);
        /* Lost-update detection: if the latest sent generation is
         * newer than what the worker just processed, OR if the outbox
         * already has new work queued, we must spawn a fresh worker
         * now. */
        if (last_sent_gen^ > gen || outbox^ != []) {
          flush_now();
        };
      };
      Js._true;
    });
}

and ensure_worker = (): Js.t(Worker.worker(Request.t, Response.t)) =>
  switch (worker_ref^) {
  | Some(w) => w
  | None =>
    let w = Worker.create(name);
    setup_worker_handlers(w);
    worker_ref := Some(w);
    w;
  }

and flush_now = (): unit => {
  /* Cancel any pending flush timer; we're flushing right now. */
  switch (flush_timer^) {
  | Some(id) =>
    Dom_html.window##clearTimeout(id);
    flush_timer := None;
  | None => ()
  };
  switch (outbox^) {
  | [] => ()
  | cells =>
    let gen = next_gen^;
    next_gen := gen + 1;
    last_sent_gen := gen;
    outbox := [];
    let req: Request.t = {
      generation: gen,
      cells,
    };
    let worker = ensure_worker();
    worker##postMessage(req);
  };
}

and schedule_flush = (): unit =>
  switch (flush_timer^) {
  | Some(_) => ()
  | None =>
    let id =
      Dom_html.window##setTimeout(
        Js.wrap_callback(() => {
          flush_timer := None;
          flush_now();
        }),
        flush_delay_ms,
      );
    flush_timer := Some(id);
  };

/* Editor entry point. Stash the request in the outbox; the debounced
 * flusher posts it to the worker. Keys -> outbox -> debounced postMessage.
 *
 * No `~timeout` callback — per-cell timeouts are now reported as
 * `Invalid("Timeout")` results inside the normal Done payload, so
 * callers don't need a separate timeout path. */
let request =
    (cells: list((string, Request.value)), ~handler: response_handler)
    : unit =>
  switch (cells) {
  | [] => ()
  | _ =>
    set_response_handler(handler);
    outbox := merge_outbox(cells);
    schedule_flush();
  };
