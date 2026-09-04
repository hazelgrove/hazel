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
/* matches the arrival stagger (Animation) so every new element gets seen */
let dwell_per_unit_ms = 320.;
let dwell_max_ms = 4500.;
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
/* agent beats: new elements arrive one by one, then existing ones make
   room slowly (a relayout that lands with the new thing hides it) */
let arrival_stagger_ms = 320;
/* a combined insertion's types bloom this far apart (the canvas's own
   schedule; terminals ride with their type) */
let arrival_step_ms = 700;
let relayout_ms = 550;
/* a tool's snapshot is captured before statics run, so its content is
   the NEXT distinct state; hold the labeled beat at most this long for
   that state to attach */
let pending_hold_ms = 4000.;
let burst_window_ms = AgentPulse.burst_window_ms;
let queue_cap = 8; /* max pending beats; middles coalesce away */

let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();

/* single source of truth lives in AgentPulse (dependency-free, so
   statics/eval can read it); this alias keeps local reads terse */
let last_agent_action = AgentPulse.last_action;
/* where the score is taking the avatar (board coords); while pacing this
   beats the tool-target hop, so the actor never bounces back to the
   enclosing definition between acts */
let avatar_site: ref(option((float, float))) = ref(None);
/* while a score plays, nothing else is released and the avatar's generic
   hop is not restaged: the score owns the stage until this time */
let hold_until: ref(float) = ref(0.);
let score_playing = (): bool => now() < hold_until^;

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
      avatar_site := None;
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
  /* statics were empty at capture: the beat is a label waiting for the
     content state that follows it */
  b_pending: bool,
  b_queued: float,
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
/* the tool label of the beat now on screen, for attribution */
let shown_label: ref(option(string)) = ref(None);

/* manual trials (__canvasBurst) embody an avatar for a while */
let fake_busy_until: ref(float) = ref(0.);
let fake_busy = (): bool => now() < fake_busy_until^;

/* ---- per-turn statistics (A4/E1): where the agent's time goes ---- */
type turn_stats = {
  mutable t_busy: float,
  mutable t_first_tool: option(float),
  mutable tools: int,
  mutable change_ms: float, /* time the canvas showed a structural change */
  mutable last_change: option(float),
  mutable longest_quiet: float,
  mutable lags: list(float), /* queued -> shown, per content beat */
  mutable turn_no: int,
};
let turn: ref(option(turn_stats)) = ref(None);
let turn_summaries: ref(list(string)) = ref([]);
let note_agent_busy = (busy: bool): unit => {
  CanvasTrajectory.busy(busy);
  switch (busy, turn^) {
  | (true, None) =>
    turn :=
      Some({
        t_busy: now(),
        t_first_tool: None,
        tools: 0,
        change_ms: 0.,
        last_change: None,
        longest_quiet: 0.,
        lags: [],
        turn_no: CanvasLog.turn_no(),
      })
  | (false, Some(t)) =>
    let end_ = now();
    let quiet_tail =
      switch (t.last_change) {
      | Some(lc) => end_ -. lc
      | None => end_ -. t.t_busy
      };
    let longest = max(t.longest_quiet, quiet_tail);
    let busy_s = (end_ -. t.t_busy) /. 1000.;
    let n = List.length(t.lags);
    let summary =
      Printf.sprintf(
        "turn %d summary: busy %.1fs | think before first tool %s | %d tool(s) | canvas change %.1fs (%.0f%%) | longest quiet %.1fs | lag %s",
        t.turn_no,
        busy_s,
        switch (t.t_first_tool) {
        | Some(f) => Printf.sprintf("%.1fs", (f -. t.t_busy) /. 1000.)
        | None => "-"
        },
        t.tools,
        t.change_ms /. 1000.,
        busy_s > 0. ? 100. *. t.change_ms /. 1000. /. busy_s : 0.,
        longest /. 1000.,
        n == 0
          ? "-"
          : Printf.sprintf(
              "avg %.1fs max %.1fs",
              List.fold_left((+.), 0., t.lags) /. float_of_int(n) /. 1000.,
              List.fold_left(max, 0., t.lags) /. 1000.,
            ),
      );
    CanvasLog.log(summary);
    turn_summaries := [summary, ...turn_summaries^];
    turn := None;
  | _ => ()
  };
};
let note_tool = (): unit =>
  switch (turn^) {
  | Some(t) =>
    t.tools = t.tools + 1;
    if (t.t_first_tool == None) {
      t.t_first_tool = Some(now());
    };
  | None => ()
  };
/* a content beat is on screen for dwell ms, queued lag ms ago */
let note_change = (~dwell: float, ~lag: float): unit =>
  switch (turn^) {
  | Some(t) =>
    let tnow = now();
    switch (t.last_change) {
    | Some(lc) => t.longest_quiet = max(t.longest_quiet, tnow -. lc)
    | None => t.longest_quiet = max(t.longest_quiet, tnow -. t.t_busy)
    };
    t.last_change = Some(tnow);
    t.change_ms = t.change_ms +. dwell;
    t.lags = [lag, ...t.lags];
  | None => ()
  };

let in_burst = (): bool => now() -. last_agent_action^ < burst_window_ms;

/* current canvas CSS zoom, set each render by CanvasSidebar; staged
   animations divide their deltas by it */
let canvas_zoom: ref(float) = ref(1.);

/* FLIP staging for a beat: graph elements at edit pace, the avatar on
   the slow action so its hop reads as travel. ~lead: agent beats hold
   the graph still while the avatar (and camera) travel, then act */
/* the avatar's screen center just before a beat renders: the tour that
   enacts the beat starts from here (its FLIP hop is replaced) */
/* the avatar box sits above-right of its site (board px); the view, the
   player and the start capture below all use the same offset */
let avatar_dx = 14.;
let avatar_dy = (-34.);
let avatar_prev: ref(option((float, float))) =
  ref(None: option((float, float)));
/* ~slow: a non-agent relayout that should still glide (the frame
   re-deriving when a burst settles) */
let stage_beat = (~lead: bool=false, ~slow: bool=false, ()): unit => {
  let scale = canvas_zoom^;
  /* where the avatar visibly IS, as a board SITE: its static anchor plus
     any in-flight translate, minus the box offset. Board units, so a
     camera move between capture and play cannot bend the path start. */
  avatar_prev :=
    Util.JsUtil.get_elem_by_id_opt("canvas-avatar")
    |> Option.map(el => {
         let px = (v: string) =>
           switch (float_of_string_opt(String.trim(v))) {
           | Some(f) => f
           | None =>
             let n = String.length(v);
             n > 2
               ? Option.value(
                   ~default=0.,
                   float_of_string_opt(String.sub(v, 0, n - 2)),
                 )
               : 0.;
           };
         let st = Js.Unsafe.get(el, "style");
         let left = px(Js.to_string(Js.Unsafe.get(st, "left")))
         and top = px(Js.to_string(Js.Unsafe.get(st, "top")));
         let tf: string =
           Js.to_string(
             Js.Unsafe.get(
               Js.Unsafe.meth_call(
                 Js.Unsafe.global##.window,
                 "getComputedStyle",
                 [|Js.Unsafe.inject(el)|],
               ),
               "transform",
             ),
           );
         /* matrix(a, b, c, d, tx, ty) */
         let (tx, ty) =
           switch (String.index_opt(tf, '(')) {
           | Some(i) when String.length(tf) > i + 1 =>
             let inner = String.sub(tf, i + 1, String.length(tf) - i - 2);
             let nums =
               inner
               |> String.split_on_char(',')
               |> List.filter_map(s => float_of_string_opt(String.trim(s)));
             switch (nums) {
             | [_, _, _, _, tx, ty] => (tx, ty)
             | _ => (0., 0.)
             };
           | _ => (0., 0.)
           };
         (left +. tx -. avatar_dx, top +. ty -. avatar_dy);
       });
  let delay = lead ? lead_ms : 0;
  let stagger = lead ? arrival_stagger_ms : 0;
  let move_dur = lead || slow ? relayout_ms : 125;
  /* while a score plays, nothing is staged: a staged beat makes the next
     render re-run the position animation on every node, which replaces a
     pending grow-in and flashes the node before its act. The beat that
     ends the hold is staged again when it is released. */
  if (!score_playing()) {
    /* elements that don't exist yet arrive staggered; edge/formation/orbit
       geometry morphs on the movers' timing instead of snapping */
    Animation.request_beat(
      ~arrival_prefixes=["cnode-", "cedge-", "cval-"],
      ~geom_prefixes=["cpath-", "cform-", "corbit-", "clead-", "cdep-"],
      ~delay,
      ~stagger,
      ~move_dur,
    );
    Animation.request(
      (
        Util.JsUtil.ids_with_prefix("cnode-")
        @ Util.JsUtil.ids_with_prefix("cedge-")
        @ Util.JsUtil.ids_with_prefix("cval-")
        |> List.map(
             Animation.Actions.move(~scale, ~delay, ~stagger, ~move_dur),
           )
      )
      @ (
        /* a playing score owns the avatar's path; a generic hop would
           replace it mid-flight */
        Util.JsUtil.ids_with_prefix("canvas-avatar")
        |> List.map(Animation.Actions.move_slow(~scale))
      ),
    );
  };
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
  note_tool();
  let before = List.length(queue^) + 1;
  queue :=
    coalesce(
      queue^
      @ [
        {
          b_model: m,
          b_label: label == "" ? None : Some(label),
          b_avatar: avatar,
          b_pending: Haz3lcore.Id.Map.is_empty(m.statics.info_map),
          b_queued: now(),
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
        let rev_q = List.rev(queue^);
        let tail_model =
          switch (rev_q) {
          | [last, ..._] => last.b_model
          | [] => sh
          };
        /* refresh the freshest holder in place (keeps dynamics current,
           keeps the tool label) */
        let refresh_tail = () =>
          switch (rev_q) {
          | [last, ...rev_rest] =>
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
        if (tail_model.statics === live.statics) {
          refresh_tail();
        } else {
          switch (rev_q) {
          | [last, ...rev_rest] when last.b_pending =>
            /* the tool's snapshot had no statics: THIS is its content */
            queue :=
              List.rev([
                {
                  ...last,
                  b_model: live,
                  b_pending: false,
                },
                ...rev_rest,
              ]);
            CanvasLog.log(
              Printf.sprintf(
                "content attached to [%s]",
                Option.value(~default="?", last.b_label),
              ),
            );
          | _ when weight(tail_model, live) == 0 =>
            /* statics identity churns on eval ticks; nothing the canvas
               would draw differently, so don't spend a beat on it */
            refresh_tail()
          | _ =>
            queue :=
              coalesce(
                queue^
                @ [
                  {
                    b_model: live,
                    b_label: None,
                    b_avatar: None,
                    b_pending: false,
                    b_queued: t,
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
    | [next, ...rest]
        when
          t
          -. last_beat^ >= due
          && t >= hold_until^
          && !(next.b_pending && t -. next.b_queued < pending_hold_ms) =>
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
        shown_label := next.b_label;
        if (w > 0) {
          note_change(~dwell=cur_dwell^, ~lag=t -. next.b_queued);
        };
        switch (next.b_label) {
        | Some(l) =>
          last_toast := Some((l, t));
          /* repaint when the toast is due to appear */
          schedule_tick(toast_delay_ms +. 20.);
        | None => ()
        };
        /* only a beat with canvas content moves the avatar; a bare tool
           beat (mark_subtask, place_probe) is not a place to go */
        switch (next.b_avatar) {
        | Some(_) as a when w > 0 => beat_avatar := a
        | _ => ()
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
      schedule_tick(
        max(60., max(due -. (t -. last_beat^), hold_until^ -. t) +. 20.),
      );
    };
    Option.value(~default=live, shown^);
  };

let tick_fired = (): unit => tick_pending := false;

/* the choreography for the beat just shown needs at least this long */
let extend_dwell = (ms: float): unit => {
  let before = cur_dwell^;
  cur_dwell := min(dwell_max_ms, max(cur_dwell^, ms));
  /* the score's full length, uncompressible by the backlog */
  hold_until := max(hold_until^, now() +. ms);
  switch (turn^) {
  | Some(t) => t.change_ms = t.change_ms +. (cur_dwell^ -. before)
  | None => ()
  };
};

/* the canvas should trust beat-carried avatar sites while beats are
   what's on screen */
let pacing_live = (): bool => in_burst() || queue^ != [] || score_playing();
