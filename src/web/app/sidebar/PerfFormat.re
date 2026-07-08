open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* Shared formatting + table helpers for the profiling debug sections
   (Evaluation, Statics, Editor & Memory, Frame Timing). Mirrors the display
   idiom of WorkerMessagingSection: an em dash for a metric that didn't run,
   durations via Core.Time_ns.Span, sizes via Core.Byte_units. */

let opt_str = (to_string: 'a => string, x: option('a)): string =>
  switch (x) {
  | None => {|—|}
  | Some(x) => to_string(x)
  };

let span = (s: Core.Time_ns.Span.t): string =>
  Core.Time_ns.Span.to_string_hum(~decimals=2, s);
let span_str = opt_str(span);

let bytes = (b: Core.Byte_units.t): string =>
  Core.Byte_units.to_string_hum(b);
let bytes_str = opt_str(bytes);

/* A header cell with hover text (title) explaining what the column measures. */
let head = ((label, tooltip): (string, string)): Node.t =>
  Node.td(
    ~attrs=[clss(["perf-head"]), Attr.title(tooltip)],
    [text(label)],
  );

let head_row = (cols: list((string, string))): Node.t =>
  Node.tr(List.map(head, cols));

/* Truncate a possibly-long label; the full text goes on the cell's tooltip. */
let truncate = (n: int, s: string): string =>
  String.length(s) <= n ? s : String.sub(s, 0, n) ++ {|…|};

/* The edit action label a frame's perform stage carries (— when none). */
let action_of = (p: option((string, Core.Time_ns.Span.t))): string =>
  switch (p) {
  | None => {|—|}
  | Some((a, _)) => a
  };

/* A left-aligned, truncated action cell with the full label on its tooltip. */
let action_cell = (label: string): Node.t =>
  Node.td(
    ~attrs=[clss(["perf-action"]), Attr.title(label)],
    [text(truncate(16, label))],
  );

let int_cell = (n: int): Node.t => Node.td([text(string_of_int(n))]);

/* Largest span in a column/table (zero if empty), for the heat-map scale. */
let max_span =
    (spans: list(option(Core.Time_ns.Span.t))): Core.Time_ns.Span.t =>
  List.fold_left(
    (acc: Core.Time_ns.Span.t, s: option(Core.Time_ns.Span.t)) =>
      switch (s) {
      | None => acc
      | Some(x) => Core.Time_ns.Span.compare(x, acc) > 0 ? x : acc
      },
    Core.Time_ns.Span.zero,
    spans,
  );

/* Flame-graph tint: an inline red background whose opacity scales with the
   span relative to `max` — transparent (untinted) for quick, red for slow. An
   alpha overlay (rather than a solid color) keeps text legible in both the
   light and dark sidebar themes. */
let heat_style = (~max: Core.Time_ns.Span.t, s: Core.Time_ns.Span.t): string => {
  let m = Core.Time_ns.Span.to_ms(max);
  let v = Core.Time_ns.Span.to_ms(s);
  let frac = m <= 0.0 ? 0.0 : v /. m;
  let frac = frac < 0.0 ? 0.0 : frac > 1.0 ? 1.0 : frac;
  Printf.sprintf("background-color: rgba(210, 45, 45, %.3f)", frac *. 0.8);
};

/* A timing cell tinted by its duration relative to `max`. `cls` classes are
   applied alongside the tint (e.g. ["perf-total"] for the bold total). */
let heat_cell =
    (
      ~max: Core.Time_ns.Span.t,
      ~cls: list(string)=[],
      s: option(Core.Time_ns.Span.t),
    )
    : Node.t =>
  switch (s) {
  | None => Node.td(~attrs=[clss(cls)], [text(span_str(None))])
  | Some(s) =>
    Node.td(
      ~attrs=[clss(cls), Attr.create("style", heat_style(~max, s))],
      [text(span(s))],
    )
  };

/* A scrollable table wrapper (columns can overflow the narrow sidebar). */
let table = (rows: list(Node.t)): Node.t =>
  div(
    ~attrs=[clss(["perf-scroll"])],
    [Node.table(~attrs=[clss(["perf-table"])], rows)],
  );

let empty = (msg: string): Node.t =>
  div(~attrs=[clss(["perf-empty"])], [text(msg)]);

let note = (msg: string): Node.t =>
  div(~attrs=[clss(["perf-note"])], [text(msg)]);
