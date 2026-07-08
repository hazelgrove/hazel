/* Data for the per-frame profiling panels (Statics, Editor & Memory, Frame
 * Timing): where a keystroke's time goes, plus cheap structural counts of the
 * current editor. Timings are captured at web-side call boundaries in
 * CodeEditable / CodeWithStatics / Page.Update.calculate / History — core is
 * untouched, and no byte-sizing heap walks run here.
 *
 * Gated by `enabled` (synced from settings in Page.Update.calculate via `sync`)
 * so nothing is measured while every per-frame panel is collapsed. */

let enabled = ref(false);
let sync = (~enabled as is_enabled: bool): unit => enabled := is_enabled;

/* Time f, hand its span to `record`, and return f's result. Only times when
 * profiling is on, so call sites needn't duplicate the wrapped call. */
let stage: 'a. (Core.Time_ns.Span.t => unit, unit => 'a) => 'a =
  (record, f) =>
    if (enabled^) {
      let (span, x) = Util.TimeUtil.timed(f);
      record(span);
      x;
    } else {
      f();
    };

/* One keystroke's timeline plus a snapshot of the editor's counts at that
 * frame, so the Statics / Editor & Memory panels can show per-run history. A
 * timing stage is None if it didn't run this frame (e.g. statics deferred by
 * the debounce). `perform` also carries the edit action. */
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
  statics_mode: string,
  segment_tokens: int,
  tiles: int,
  rows: int,
  projectors: int,
  backpack: int,
};

/* Mutable builder for the frame under construction; committed by end_calc. */
type builder = {
  mutable perform: option((string, Core.Time_ns.Span.t)),
  mutable statics: option(Core.Time_ns.Span.t),
  mutable syntax: option(Core.Time_ns.Span.t),
  mutable cursor_info: option(Core.Time_ns.Span.t),
  mutable color_map: option(Core.Time_ns.Span.t),
};
let building: builder = {
  perform: None,
  statics: None,
  syntax: None,
  cursor_info: None,
  color_map: None,
};

let history_limit = 30;
let frames: ref(list(frame)) = ref([]); /* newest first */

/* Cheap structural counts of the most recently calculated editor, the statics
 * recompute mode, and undo/redo depth — not per-frame timeline data. */
type counts = {
  mutable info_map_entries: int,
  mutable error_count: int,
  mutable warning_count: int,
  mutable statics_mode: string,
  mutable segment_tokens: int,
  mutable tiles: int,
  mutable rows: int,
  mutable projectors: int,
  mutable backpack: int,
  mutable undo_depth: int,
  mutable redo_depth: int,
};
let counts: counts = {
  info_map_entries: 0,
  error_count: 0,
  warning_count: 0,
  statics_mode: {|—|},
  segment_tokens: 0,
  tiles: 0,
  rows: 0,
  projectors: 0,
  backpack: 0,
  undo_depth: 0,
  redo_depth: 0,
};

/* --- timeline recorders (called only when enabled, via `stage`) --- */
/* Accumulate within a frame: a stage that runs more than once (e.g. statics
 * across several editors in exercise modes) sums, so the frame total is right.
 * begin_calc resets each stage to None at the frame start. */
let add =
    (cur: option(Core.Time_ns.Span.t), span: Core.Time_ns.Span.t)
    : option(Core.Time_ns.Span.t) =>
  switch (cur) {
  | None => Some(span)
  | Some(s) => Some(Core.Time_ns.Span.(s + span))
  };

let record_perform = (~action: string, span) =>
  building.perform = Some((action, span));
let record_statics = span => building.statics = add(building.statics, span);
let record_syntax = span => building.syntax = add(building.syntax, span);
let record_cursor = span =>
  building.cursor_info = add(building.cursor_info, span);
let record_colors = span =>
  building.color_map = add(building.color_map, span);

/* Like `stage`, but records the perform stage with the triggering action.
 * `action` is a thunk so the (possibly costly) label is only built when on. */
let stage_perform: 'a. (~action: unit => string, unit => 'a) => 'a =
  (~action, f) =>
    if (enabled^) {
      let (span, x) = Util.TimeUtil.timed(f);
      record_perform(~action=action(), span);
      x;
    } else {
      f();
    };

/* Reset the calculate-phase stages at the start of a calculate; `perform` is
 * left as the preceding update phase set it (that update belongs to this
 * frame). */
let begin_calc = (): unit =>
  if (enabled^) {
    building.statics = None;
    building.syntax = None;
    building.cursor_info = None;
    building.color_map = None;
  };

/* Commit the frame (snapshotting the counts recorded during this frame's editor
 * calculate), then clear perform for the next. `calc` is the calculate-phase
 * wall clock; the reported total is perform + calc, since the edit action's
 * perform runs in the earlier update phase — so total is always >= perform. */
let end_calc = (~calc: Core.Time_ns.Span.t): unit =>
  if (enabled^) {
    let total =
      switch (building.perform) {
      | Some((_, p)) => Core.Time_ns.Span.(p + calc)
      | None => calc
      };
    let f = {
      perform: building.perform,
      statics: building.statics,
      syntax: building.syntax,
      cursor_info: building.cursor_info,
      color_map: building.color_map,
      total: Some(total),
      info_map_entries: counts.info_map_entries,
      errors: counts.error_count,
      warnings: counts.warning_count,
      statics_mode: counts.statics_mode,
      segment_tokens: counts.segment_tokens,
      tiles: counts.tiles,
      rows: counts.rows,
      projectors: counts.projectors,
      backpack: counts.backpack,
    };
    frames := [f, ...Util.ListUtil.take(history_limit - 1, frames^)];
    building.perform = None;
  };

/* --- count recorders (called directly from sites) --- */
let record_statics_counts =
    (~info_map_entries, ~error_count, ~warning_count, ~statics_mode): unit =>
  if (enabled^) {
    counts.info_map_entries = info_map_entries;
    counts.error_count = error_count;
    counts.warning_count = warning_count;
    counts.statics_mode = statics_mode;
  };

let record_editor_counts =
    (~segment_tokens, ~tiles, ~rows, ~projectors, ~backpack): unit =>
  if (enabled^) {
    counts.segment_tokens = segment_tokens;
    counts.tiles = tiles;
    counts.rows = rows;
    counts.projectors = projectors;
    counts.backpack = backpack;
  };

let record_history = (~undo_depth, ~redo_depth): unit =>
  if (enabled^) {
    counts.undo_depth = undo_depth;
    counts.redo_depth = redo_depth;
  };
