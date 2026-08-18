/* Collector for the per-frame profiling panels; see PerfMetrics.re. The stage
 * timers and recorders are self-gating, so callers never check whether a panel
 * is open. */

type statics_mode =
  | Recomputed
  | Forced
  | Deferred
  | Cached;

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
  statics_mode: option(statics_mode),
  segment_tokens: int,
  tiles: int,
  rows: int,
  projectors: int,
};

type live = {
  undo_depth: int,
  redo_depth: int,
  backpack: int,
};

/* Turn collection on/off from settings; called once per update cycle. */
let sync: (~enabled: bool) => unit;

/* Recent frames, newest first. */
let history: ref(list(frame));

/* Depths that only mean anything now, not per frame. */
let live: ref(live);

/* Wrap one update cycle's calculate phase; commits a frame for it. */
let time_frame: (unit => 'a) => 'a;

/* Wrap a stage, recording how long it took on the frame under construction. */
let time_perform: (~action: Haz3lcore.Action.t, unit => 'a) => 'a;
let time_statics: (unit => 'a) => 'a;
let time_syntax: (unit => 'a) => 'a;
let time_cursor: (unit => 'a) => 'a;
let time_colors: (unit => 'a) => 'a;

let record_statics_counts:
  (~recompute: bool, ~mode: StaticsMode.t, Haz3lcore.CachedStatics.t) => unit;
let record_syntax_counts: Haz3lcore.CachedSyntax.t => unit;
let record_history: (~undo: list('a), ~redo: list('b)) => unit;
