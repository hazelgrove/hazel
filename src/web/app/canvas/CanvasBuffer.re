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

/* Called from the agent tool executor on every applied tool call. */
let note_agent_action = (): unit => last_agent_action := now();

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
let push_snapshot = (m: CodeWithStatics.Model.t): unit => {
  note_agent_action();
  queue := coalesce(queue^ @ [m]);
};

let reset = (): unit => {
  queue := [];
  shown := None;
  last_seen := None;
  tick_pending := false;
};

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
        };
      } else {
        shown := Some(live);
        last_beat := t;
        queue := [];
      };
    };
    switch (queue^) {
    | [next, ...rest] when t -. last_beat^ >= cadence_ms =>
      stage_beat();
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
