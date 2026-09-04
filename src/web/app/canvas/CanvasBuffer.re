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

/* dwell = how long a shown beat holds before the next releases:
   weighted by the size of the change it introduced (a five-definition
   insert earns a look, a rename doesn't), then compressed elastically as
   the backlog grows — the display speeds up BEFORE it drops anything */
let dwell_base_ms = 450.;
let dwell_per_unit_ms = 250.;
let dwell_max_ms = 2000.;
let dwell_min_ms = 250.;
let dwell_of = (weight: int): float =>
  min(
    dwell_max_ms,
    dwell_base_ms +. dwell_per_unit_ms *. float_of_int(weight),
  );
let elastic = (dwell: float, pending: int): float =>
  max(
    dwell_min_ms,
    dwell /. (1. +. 0.5 *. float_of_int(max(0, pending - 1))),
  );
/* the graph's own elements move this long after the avatar sets off, so
   a beat reads travel -> act -> settle instead of everything at once */
let lead_ms = 260;
let burst_window_ms = AgentPulse.burst_window_ms;
let queue_cap = 8; /* max pending beats; middles coalesce away */

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

/* single source of truth lives in AgentPulse (dependency-free, so
   statics/eval can read it); this alias keeps local reads terse */
let last_agent_action = AgentPulse.last_action;

/* Canvas-authoring gestures (place/connect stubs) ride the agent's
   DirectEdit tool path; they must NOT read as agent activity or every
   manual gesture triggers pacing + auto-fit zoom hops. */
let suppress_stamp: ref(bool) = ref(false);

/* Called from the agent tool executor on every applied tool call. */
let note_agent_action = (): unit =>
  if (! suppress_stamp^) {
    if (now() -. last_agent_action^ >= burst_window_ms) {
      CanvasLog.log(
        Printf.sprintf("burst start -> turn %d", CanvasLog.next_turn()),
      );
    };
    last_agent_action := now();
  };

/* a pending beat: the snapshot plus the tool that produced it (None
   for trailing live-state enqueues) */
type beat = {
  b_model: CodeWithStatics.Model.t,
  b_label: option(string),
  /* where the producing tool acted (site id + edit/err state), captured
     at exec time so the avatar hops WITH its beat instead of reading
     live agent state and arriving before the scenery */
  b_avatar: option((Haz3lcore.Id.t, string)),
};
let queue: ref(list(beat)) = ref([]);
let shown: ref(option(CodeWithStatics.Model.t)) = ref(None);
/* (tool name, shown-at) of the most recent labeled beat, for the
   avatar's transient action toast */
let toast_ms = 1100.;
/* the toast lands in the settle phase, after the avatar has arrived */
let toast_delay_ms = 380.;
let last_toast: ref(option((string, float))) =
  ref(None: option((string, float)));
/* the avatar site of the beat currently SHOWN (sticky across beats that
   carry none, cleared when pacing disengages) */
let beat_avatar: ref(option((Haz3lcore.Id.t, string))) =
  ref(None: option((Haz3lcore.Id.t, string)));
let current_toast = (): option(string) =>
  switch (last_toast^) {
  | Some((l, t))
      when
        now() -. t >= toast_delay_ms && now() -. t < toast_delay_ms +. toast_ms =>
    Some(l)
  | _ => None
  };
let last_seen: ref(option(CodeWithStatics.Model.t)) = ref(None);
let last_beat: ref(float) = ref(0.);
/* dwell owed by the beat currently shown */
let cur_dwell: ref(float) = ref(dwell_base_ms);
let tick_pending: ref(bool) = ref(false);

let in_burst = (): bool => now() -. last_agent_action^ < burst_window_ms;

/* current canvas CSS zoom, set each render by CanvasSidebar; staged
   animations divide their deltas by it */
let canvas_zoom: ref(float) = ref(1.);

/* FLIP staging for a beat: graph elements at edit pace, the avatar on
   the slow action so its hop reads as travel. ~lead: agent beats hold
   the graph still while the avatar (and camera) travel, then act */
let stage_beat = (~lead: bool=false, ()): unit => {
  let scale = canvas_zoom^;
  let delay = lead ? lead_ms : 0;
  Animation.request(
    (
      Util.JsUtil.ids_with_prefix("cnode-")
      @ Util.JsUtil.ids_with_prefix("cedge-")
      @ Util.JsUtil.ids_with_prefix("cval-")
      |> List.map(Animation.Actions.move(~scale, ~delay))
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
let coalesce = (q: list(beat)) =>
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
let push_snapshot =
    (
      ~label: string="",
      ~avatar: option((Haz3lcore.Id.t, string))=None,
      m: CodeWithStatics.Model.t,
    )
    : unit => {
  note_agent_action();
  let before = List.length(queue^) + 1;
  queue :=
    coalesce(
      queue^
      @ [
        {
          b_model: m,
          b_label: label == "" ? None : Some(label),
          b_avatar: avatar,
        },
      ],
    );
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
      /* beats failing this test (e.g. a snapshot whose graph extraction
         comes up empty mid-burst) are dropped instead of rendered */
      ~viable: CodeWithStatics.Model.t => bool=_ => true,
      /* size of the change between two states (graph elements that
         differ), weighting the dwell of the beat that introduces it */
      ~weight: (CodeWithStatics.Model.t, CodeWithStatics.Model.t) => int=(
                                                                    _,
                                                                    _,
                                                                    ) =>
                                                                    0,
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
        Printf.sprintf(
          "burst end (%d pending, draining at cadence)",
          List.length(queue^),
        ),
      );
      /* one insurance tick: the next calculate lifts the ambient-
         sampling mask (and re-expands targets -> re-eval) even when
         the beat queue is already empty */
      schedule_tick(650.);
    };
    was_in_burst := burst;
    let fresh =
      switch (last_seen^) {
      | Some(s) => !(s === live)
      | None => true
      };
    if (fresh) {
      last_seen := Some(live);
      switch (shown^) {
      | Some(sh) when burst || queue^ != [] =>
        /* paced: only distinct-STATICS states become beats. The live
           model record is rebuilt on every app tick (streaming text,
           etc.), so physical freshness floods the queue with no-op
           states; statics identity is the content signal (the same one
           the old tail-dedup trusted). No-op rebuilds refresh the
           freshest holder in place so dynamics stay current. Once the
           burst ends, pending beats DRAIN at cadence rather than
           jump-cutting to live. */
        let tail_statics =
          switch (List.rev(queue^)) {
          | [last, ..._] => last.b_model.statics
          | [] => sh.statics
          };
        if (tail_statics === live.statics) {
          switch (List.rev(queue^)) {
          | [last, ...rev_rest] =>
            /* refresh the model, keep the tool label */
            queue :=
              List.rev([
                {
                  ...last,
                  b_model: live,
                },
                ...rev_rest,
              ])
          | [] => shown := Some(live)
          };
        } else {
          queue :=
            coalesce(
              queue^
              @ [
                {
                  b_model: live,
                  b_label: None,
                  b_avatar: None,
                },
              ],
            );
          CanvasLog.log(
            Printf.sprintf(
              "state change queued (pending %d)",
              List.length(queue^),
            ),
          );
        };
      | _ =>
        /* idle and nothing pending: live passes straight through */
        shown := Some(live);
        last_beat := t;
        queue := [];
        beat_avatar := None;
      };
    };
    let due = elastic(cur_dwell^, List.length(queue^));
    switch (queue^) {
    | [next, ...rest] when t -. last_beat^ >= due =>
      let shown_viable =
        switch (shown^) {
        | Some(sh) => viable(sh)
        | None => false
        };
      if (!viable(next.b_model) && shown_viable) {
        /* blank interstitial (the graph would vanish for a beat) */
        CanvasLog.log(
          Printf.sprintf(
            "skipped blank interstitial beat (%d still pending)",
            List.length(rest),
          ),
        );
        queue := rest;
      } else {
        stage_beat(~lead=true, ());
        let w =
          switch (shown^) {
          | Some(sh) => weight(sh, next.b_model)
          | None => 0
          };
        cur_dwell := dwell_of(w);
        CanvasLog.log(
          Printf.sprintf(
            "beat shown%s (%.1fs since last, weight %d -> dwell %.0fms, %d still pending)",
            switch (next.b_label) {
            | Some(l) => " [" ++ l ++ "]"
            | None => ""
            },
            (t -. last_beat^) /. 1000.,
            w,
            cur_dwell^,
            List.length(rest),
          ),
        );
        switch (next.b_label) {
        | Some(l) =>
          last_toast := Some((l, t));
          /* repaint when the toast is due to appear */
          schedule_tick(toast_delay_ms +. 20.);
        | None => ()
        };
        switch (next.b_avatar) {
        | Some(_) as a => beat_avatar := a
        | None => ()
        };
        shown := Some(next.b_model);
        last_beat := t;
        queue := rest;
      };
    | _ => ()
    };
    if (queue^ != [] && ! tick_pending^) {
      tick_pending := true;
      let due = elastic(cur_dwell^, List.length(queue^));
      schedule_tick(max(60., due -. (t -. last_beat^) +. 20.));
    };
    Option.value(~default=live, shown^);
  };

let tick_fired = (): unit => tick_pending := false;

/* the canvas should trust beat-carried avatar sites while beats are
   what's on screen */
let pacing_live = (): bool => in_burst() || queue^ != [];
