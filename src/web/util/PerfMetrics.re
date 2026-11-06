/* Data for the per-frame telemetry panels (Statics, Editor & Memory, Frame
 * Timing): where a keystroke's time goes, plus cheap structural counts of the
 * current editor. Timings are captured at web-side call boundaries in
 * CodeEditable / CodeWithStatics / Page.Update.calculate / History — core is
 * untouched, and no byte-sizing heap walks run here.
 *
 * Gating and the bounded history come from Metrics.Make, so nothing is measured
 * while every per-frame panel is collapsed and no call site tests for it. */

/* What became of statics on a frame — the Statics panel's `outcome` column.
 * Distinct from StaticsMode.t, which is what the throttle *asked* for; this is
 * what happened, derived from that request plus whether the recompute ran. */
[@deriving show({with_path: false})]
type statics_outcome =
  | Recomputed /* an edit landed and statics ran this frame */
  | Forced /* the debounce timer fired and forced a run */
  | Deferred /* an edit landed but the debounce postponed the run */
  | Cached; /* nothing to redo — the cached statics were reused */

/* Why the live-typing pass did or didn't run on a frame. Live typing re-runs
 * Statics.mk against the evaluated dynamics, so unlike the other stages it can
 * be skipped for reasons of its own: the streaming throttle, or nothing new to
 * type against. */
[@deriving show({with_path: false})]
type live_typing_outcome =
  | Ran /* the pass ran against fresh dynamics or a moved sample focus */
  | Throttled /* a stream slice landed inside the throttle window */
  | Reused /* neither the dynamics nor the sample focus changed */
  | Off; /* live typing is switched off in settings */

/* One keystroke's timeline plus a snapshot of the editor's counts at that
 * frame, so the Statics / Editor & Memory panels can show per-run history. A
 * timing stage is None if it didn't run this frame (e.g. statics deferred by
 * the debounce), so an absent stage reads as `—` rather than a misleading 0.
 * `perform` also carries the edit action. */
type frame = {
  perform: option((string, Core.Time_ns.Span.t)),
  statics: option(Core.Time_ns.Span.t),
  syntax: option(Core.Time_ns.Span.t),
  live_typing: option(Core.Time_ns.Span.t),
  cursor_info: option(Core.Time_ns.Span.t),
  color_map: option(Core.Time_ns.Span.t),
  total: option(Core.Time_ns.Span.t),
  info_map_entries: int,
  errors: int,
  warnings: int,
  statics_outcome: option(statics_outcome),
  live_typing_entries: int,
  live_typing_errors: int,
  live_typing_outcome: option(live_typing_outcome),
  segment_tokens: int,
  tiles: int,
  rows: int,
  projectors: int,
};

let empty_frame = {
  perform: None,
  statics: None,
  syntax: None,
  live_typing: None,
  cursor_info: None,
  color_map: None,
  total: None,
  info_map_entries: 0,
  errors: 0,
  warnings: 0,
  statics_outcome: None,
  live_typing_entries: 0,
  live_typing_errors: 0,
  live_typing_outcome: None,
  segment_tokens: 0,
  tiles: 0,
  rows: 0,
  projectors: 0,
};

include Metrics.Make({
  type t = frame;
  let limit = 30;
});

/* The frame under construction: stage timings accumulate here and the counts
 * are refreshed as each editor recalculates, until `time_frame` commits it. */
let current: ref(frame) = ref(empty_frame);

/* Depths that only mean anything "now" rather than per frame, shown as a live
 * line above the Editor & Memory table. */
type live = {
  undo_depth: int,
  redo_depth: int,
  backpack: int,
};
let live: ref(live) =
  ref({
    undo_depth: 0,
    redo_depth: 0,
    backpack: 0,
  });

/* --- stage timers --- */

/* Accumulate within a frame: a stage that runs more than once (e.g. statics
 * across several editors in exercise modes) sums, so the frame total is right.
 * time_frame resets each stage at the frame start. */
let add =
    (cur: option(Core.Time_ns.Span.t), span: Core.Time_ns.Span.t)
    : option(Core.Time_ns.Span.t) =>
  Some(
    Option.fold(~none=span, ~some=s => Core.Time_ns.Span.(s + span), cur),
  );

/* Time f, folding its span into the frame under construction with `into`, and
 * return f's result. Only times when telemetry is on, so call sites needn't
 * duplicate the wrapped call. */
let time: 'a. ((frame, Core.Time_ns.Span.t) => frame, unit => 'a) => 'a =
  (into, f) =>
    if (enabled^) {
      let (span, x) = Util.TimeUtil.timed(f);
      current := into(current^, span);
      x;
    } else {
      f();
    };

let time_statics = f =>
  time(
    (fr, s) =>
      {
        ...fr,
        statics: add(fr.statics, s),
      },
    f,
  );
let time_syntax = f =>
  time(
    (fr, s) =>
      {
        ...fr,
        syntax: add(fr.syntax, s),
      },
    f,
  );
let time_live_typing = f =>
  time(
    (fr, s) =>
      {
        ...fr,
        live_typing: add(fr.live_typing, s),
      },
    f,
  );
let time_cursor = f =>
  time(
    (fr, s) =>
      {
        ...fr,
        cursor_info: add(fr.cursor_info, s),
      },
    f,
  );
let time_colors = f =>
  time(
    (fr, s) =>
      {
        ...fr,
        color_map: add(fr.color_map, s),
      },
    f,
  );

/* Like the stage timers, but also records the triggering action. Labelling it
 * here keeps `Action.show`, which is not cheap, off the hot path when no panel
 * is open — and the frame stores the label rather than the action so it runs once
 * per frame instead of once per render. Assigns: one perform runs per update. */
let time_perform: 'a. (~action: Haz3lcore.Action.t, unit => 'a) => 'a =
  (~action, f) =>
    if (enabled^) {
      let (span, x) = Util.TimeUtil.timed(f);
      current :=
        {
          ...current^,
          perform: Some((Haz3lcore.Action.show(action), span)),
        };
      x;
    } else {
      f();
    };

/* Run one update cycle's calculate phase and commit a frame for it. The reported
 * total is perform + calculate, since the edit action's perform ran in the
 * earlier update phase — so total is always >= perform. */
let time_frame: 'a. (unit => 'a) => 'a =
  f =>
    if (enabled^) {
      /* Clear the previous frame's calculate stages; `perform` stays as the
       * preceding update phase set it, since that update belongs to this
       * frame. */
      current :=
        {
          ...current^,
          statics: None,
          syntax: None,
          live_typing: None,
          cursor_info: None,
          color_map: None,
        };
      let (calc, x) = Util.TimeUtil.timed(f);
      let frame = current^;
      let total =
        frame.perform
        |> Option.map(snd)
        |> Option.fold(~none=calc, ~some=p => Core.Time_ns.Span.(p + calc));
      push({
        ...frame,
        total: Some(total),
      });
      current :=
        {
          ...frame,
          perform: None,
        };
      x;
    } else {
      f();
    };

/* --- count recorders --- */

/* Snapshot what statics produced this frame, and why it did or didn't run:
 * `recompute` is the throttle gate's decision, `mode` the debounce's. */
let record_statics_counts =
    (
      ~recompute: bool,
      ~mode: StaticsMode.t,
      statics: Haz3lcore.CachedStatics.t,
    )
    : unit =>
  when_enabled(() => {
    let statics_outcome =
      switch (recompute, mode) {
      | (true, StaticsMode.Force) => Forced
      | (true, _) => Recomputed
      | (false, StaticsMode.Defer) => Deferred
      | (false, _) => Cached
      };
    current :=
      {
        ...current^,
        info_map_entries: Haz3lcore.Id.Map.cardinal(statics.info_map),
        errors: List.length(statics.error_ids),
        warnings: List.length(statics.warning_ids),
        statics_outcome: Some(statics_outcome),
      };
  });

/* Snapshot what the live-typing pass produced this frame, and why it did or
 * didn't run. Takes the whole CachedStatics rather than the counts so the fold
 * over the error ids and the info map stays inside the gate, as in
 * record_history. The counts describe the live typing now in effect, so a
 * skipped frame reports the values it reused, with `outcome` saying so. */
let record_live_typing_counts =
    (~outcome: live_typing_outcome, statics: Haz3lcore.CachedStatics.t): unit =>
  when_enabled(() =>
    current :=
      {
        ...current^,
        live_typing_entries:
          Haz3lcore.Id.Map.cardinal(statics.live_typing_info_map),
        live_typing_errors: List.length(statics.live_typing_error_ids),
        live_typing_outcome: Some(outcome),
      }
  );

/* Cheap structural counts of the editor this frame recalculated. Byte-exact
 * sizes are deliberately not computed: heap walks are expensive per frame, and
 * timings plus these counts answer the question. */
let record_syntax_counts = (syntax: Haz3lcore.CachedSyntax.t): unit =>
  when_enabled(() => {
    current :=
      {
        ...current^,
        segment_tokens: List.length(syntax.segment),
        tiles: Haz3lcore.Id.Map.cardinal(syntax.measured.tiles),
        rows: Haz3lcore.Measured.Rows.cardinal(syntax.measured.rows),
        projectors: List.length(syntax.projector_list),
      };
    live :=
      {
        ...live^,
        backpack: List.length(syntax.missing_shards),
      };
  });

/* Takes the stacks, not their depths: `List.length` on an uncapped undo stack is
 * O(n) and this runs every frame, so it belongs inside the gate. Polymorphic in
 * the entries so the collector needn't know History's types. */
let record_history = (~undo: list('a), ~redo: list('b)): unit =>
  when_enabled(() =>
    live :=
      {
        ...live^,
        undo_depth: List.length(undo),
        redo_depth: List.length(redo),
      }
  );
