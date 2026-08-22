/* CanvasBuffer — temporal pacing of agent actions on the canvas.

   When the agent fires several edits in quick succession (one turn with
   many tool calls, or rapid turns), the canvas would jump-cut to the
   final state. Instead, snapshots of the editor model queue up and are
   released at a maximum rate, so the FLIP animation plays each hop and
   the agent's sequence of actions is telegraphed beat by beat.

   Purely display-side: the real editor state is never delayed — only
   what the canvas panel renders from. Human editing passes through
   live (pacing engages only within a burst window after agent tool
   activity). Snapshot identity is physical equality on the editor
   model record, which is rebuilt on every state change. */

open Js_of_ocaml;

let cadence_ms = 700.; /* min interval between displayed beats */
let burst_window_ms = 5000.; /* agent activity recency to engage pacing */
let queue_cap = 4; /* max pending beats; middles coalesce away */

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

let last_agent_action: ref(float) = ref(-1.e12);

/* Canvas-authoring gestures (place/connect stubs) ride the agent's
   DirectEdit tool path; they must NOT read as agent activity or every
   manual gesture triggers pacing + auto-fit zoom hops. */
let suppress_stamp: ref(bool) = ref(false);

/* Called from the agent tool executor on every applied tool call. */
let note_agent_action = (): unit =>
  if (! suppress_stamp^) {
    if (now() -. last_agent_action^ >= burst_window_ms) {
      CanvasLog.log("burst start (agent activity)");
    };
    last_agent_action := now();
  };

let queue: ref(list(CodeWithStatics.Model.t)) = ref([]);
let shown: ref(option(CodeWithStatics.Model.t)) = ref(None);
let last_seen: ref(option(CodeWithStatics.Model.t)) = ref(None);
let last_beat: ref(float) = ref(0.);
let tick_pending: ref(bool) = ref(false);

let in_burst = (): bool => now() -. last_agent_action^ < burst_window_ms;

/* current canvas CSS zoom, set each render by CanvasSidebar; staged
   animations divide their deltas by it */
let canvas_zoom: ref(float) = ref(1.);

let last_autofit: ref(float) = ref(0.);
let autofit_due = (): bool =>
  if (now() -. last_autofit^ > 900.) {
    last_autofit := now();
    true;
  } else {
    false;
  };

/* FLIP staging for a beat: graph elements at edit pace, the avatar on
   the slow action so its hop reads as travel */
let stage_beat = (): unit => {
  let scale = canvas_zoom^;
  Animation.request(
    (
      Util.JsUtil.ids_with_prefix("cnode-")
      @ Util.JsUtil.ids_with_prefix("cedge-")
      @ Util.JsUtil.ids_with_prefix("cval-")
      |> List.map(Animation.Actions.move(~scale))
    )
    @ (
      Util.JsUtil.ids_with_prefix("canvas-avatar")
      |> List.map(Animation.Actions.move_slow(~scale))
    ),
  );
};

/* Drop middle beats when over cap: keep the oldest pending (continuity
   from what is shown) and the newest (never fall behind the truth by
   more than the cap). */
let coalesce = (q: list(CodeWithStatics.Model.t)) =>
  switch (q) {
  | [_, ..._] when List.length(q) > queue_cap =>
    switch (q, List.rev(q)) {
    | ([first, ...mid], [last, ..._]) =>
      let keep_mid =
        mid
        |> List.filteri((i, _) => i >= List.length(mid) - (queue_cap - 2));
      [first, ...keep_mid]
      @ (
        switch (keep_mid) {
        | [] => [last]
        | _ => []
        }
      );
    | _ => q
    }
  | q => q
  };

/* Execution-time capture: a multi-tool reply runs all its calls inside
   ONE app action, so only the final state ever renders — the canvas
   would collapse N definitions into one beat. Each applied tool call
   pushes its intermediate editor model here. */
let push_snapshot = (~label: string="", m: CodeWithStatics.Model.t): unit => {
  note_agent_action();
  let before = List.length(queue^) + 1;
  queue := coalesce(queue^ @ [m]);
  let after = List.length(queue^);
  CanvasLog.log(
    Printf.sprintf(
      "tool %s -> beat queued (pending %d%s)",
      label == "" ? "?" : label,
      after,
      before > after
        ? Printf.sprintf(", coalesced away %d", before - after) : "",
    ),
  );
};

let reset = (): unit => {
  queue := [];
  shown := None;
  last_seen := None;
  tick_pending := false;
};

let was_in_burst: ref(bool) = ref(false);

/* Returns the model the canvas should render; schedules a re-render
   tick while beats remain pending. */
let observe =
    (
      ~enabled: bool,
      ~schedule_tick: float => unit,
      live: CodeWithStatics.Model.t,
    )
    : CodeWithStatics.Model.t =>
  if (!enabled) {
    reset();
    live;
  } else {
    let t = now();
    let burst = in_burst();
    if (was_in_burst^ && !burst) {
      CanvasLog.log(
        Printf.sprintf("burst end (quiet %.0fs)", burst_window_ms /. 1000.),
      );
    };
    was_in_burst := burst;
    let fresh =
      switch (last_seen^) {
      | Some(s) => !(s === live)
      | None => true
      };
    if (fresh) {
      last_seen := Some(live);
      /* the final rendered state usually equals the last exec-time push
         (same statics ref) — don't double-queue it */
      let already_queued =
        switch (List.rev(queue^)) {
        | [last, ..._] => last.statics === live.statics
        | [] => false
        };
      if (in_burst() && shown^ != None) {
        if (!already_queued) {
          queue := coalesce(queue^ @ [live]);
          CanvasLog.log(
            Printf.sprintf(
              "state change queued (pending %d)",
              List.length(queue^),
            ),
          );
        };
      } else {
        if (queue^ != []) {
          CanvasLog.log(
            Printf.sprintf(
              "pacing flushed: jumped to live state (dropped %d pending)",
              List.length(queue^),
            ),
          );
        };
        shown := Some(live);
        last_beat := t;
        queue := [];
      };
    };
    switch (queue^) {
    | [next, ...rest] when t -. last_beat^ >= cadence_ms =>
      stage_beat();
      CanvasLog.log(
        Printf.sprintf(
          "beat shown (%.1fs since last, %d still pending)",
          (t -. last_beat^) /. 1000.,
          List.length(rest),
        ),
      );
      shown := Some(next);
      last_beat := t;
      queue := rest;
    | _ => ()
    };
    if (queue^ != [] && ! tick_pending^) {
      tick_pending := true;
      schedule_tick(max(60., cadence_ms -. (t -. last_beat^) +. 20.));
    };
    Option.value(~default=live, shown^);
  };

let tick_fired = (): unit => tick_pending := false;
