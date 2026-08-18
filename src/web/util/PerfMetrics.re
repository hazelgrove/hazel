/* Data for the per-frame profiling panels (Statics, Editor & Memory, Frame
 * Timing): where a keystroke's time goes, plus cheap structural counts of the
 * current editor. Timings are captured at web-side call boundaries in
 * CodeEditable / CodeWithStatics / Page.Update.calculate / History — core is
 * untouched, and no byte-sizing heap walks run here.
 *
 * Gating and the bounded history come from Metrics.Make, so nothing is measured
 * while every per-frame panel is collapsed and no call site tests for it. */

/* What actually became of statics on a frame — the Statics panel's `outcome`
 * column. Distinct from StaticsMode.t, which is what the throttle *asked* for
 * (Normal | Defer | Force); this is what happened, derived from that request
 * plus whether the recompute ran. `show`n rather than hand-mapped to strings. */
[@deriving show({with_path: false})]
type statics_outcome =
  | Recomputed /* an edit landed and statics ran this frame */
  | Forced /* the debounce timer fired and forced a run */
  | Deferred /* an edit landed but the debounce postponed the run */
  | Cached; /* nothing to redo — the cached statics were reused */

/* One keystroke's timeline plus a snapshot of the editor's counts at that
 * frame, so the Statics / Editor & Memory panels can show per-run history. A
 * timing stage is None if it didn't run this frame (e.g. statics deferred by
 * the debounce), so an absent stage reads as `—` rather than a misleading 0.
 * `perform` also carries the edit action. */
type frame = {
  perform: option((string, Core.Time_ns.Span.t)),
  statics: option(Core.Time_ns.Span.t),
  syntax: option(Core.Time_ns.Span.t),
  cursor_info: option(Core.Time_ns.Span.t),
  color_map: option(Core.Time_ns.Span.t),
  total: option(Core.Time_ns.Span.t),
  info_map_entries: int,
  errors: int,
  warnings: int,
  statics_outcome: option(statics_outcome),
  segment_tokens: int,
  tiles: int,
  rows: int,
  projectors: int,
};

let empty_frame = {
  perform: None,
  statics: None,
  syntax: None,
  cursor_info: None,
  color_map: None,
  total: None,
  info_map_entries: 0,
  errors: 0,
  warnings: 0,
  statics_outcome: None,
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
 * return f's result. Only times when profiling is on, so call sites needn't
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

/* Like the stage timers, but also records the triggering action. Takes the
 * action itself and labels it here, so the (not cheap) `Action.show` runs only
 * while a panel is open — and once per frame rather than once per render, which
 * is why the frame keeps the label and not the action. Assigns rather than
 * accumulating: one perform runs per update. */
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

/* Run one update cycle's calculate phase and commit a frame for it. The
 * reported total is perform + calculate, since the edit action's perform ran in
 * the earlier update phase — so total is always >= perform. Owning both
 * boundaries here means neither can be forgotten at the call site. */
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

/* Snapshot what statics produced this frame, and why it did or didn't run.
 * `recompute` is the throttle gate's decision and `mode` the debounce's, so the
 * reported mode is derived here rather than at the call site. */
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
        backpack: List.length(syntax.cached_backpack),
      };
  });

/* Takes the stacks rather than their depths: `List.length` on an uncapped undo
 * stack is O(n) and this runs every frame, so it must happen inside the gate.
 * Polymorphic in the entries, so the collector needn't know History's types. */
let record_history = (~undo: list('a), ~redo: list('b)): unit =>
  when_enabled(() =>
    live :=
      {
        ...live^,
        undo_depth: List.length(undo),
        redo_depth: List.length(redo),
      }
  );
