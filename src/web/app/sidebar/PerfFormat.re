open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* Shared formatting and table building for the instrumented debug sections
   (Worker Messaging, Evaluation, Statics, Editor & Memory, Frame Timing): an em
   dash for a metric that didn't run, durations via Core.Time_ns.Span, sizes via
   Core.Byte_units, and the flame-graph heat map.

   A table is described as data — a list of `column`s, each carrying its header,
   the tooltip explaining what it measures, and how to render one row's cell — so
   a header and its cells cannot drift apart, and every section's table is built
   by the single `table` below. Sections only choose columns and hand over rows;
   all HTML lives here. */

/* An em dash rather than a misleading 0 for a metric that didn't run. */
let fmt_opt = (to_string: 'a => string, x: option('a)): string =>
  switch (x) {
  | None => {|—|}
  | Some(x) => to_string(x)
  };

let fmt_span = (s: Core.Time_ns.Span.t): string =>
  Core.Time_ns.Span.to_string_hum(~decimals=2, s);

let fmt_bytes = (b: Core.Byte_units.t): string =>
  Core.Byte_units.to_string_hum(b);

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

/* Below this a timing reads as instant (no tint), so fast frames stay pale no
   matter how they compare to each other. */
let heat_floor_ms = 0.5;
/* The red end of the scale never anchors below this, so as long as everything
   is under ~100ms nothing goes deep red; once something is slower, the anchor
   stretches out to the visible max. */
let heat_ceil_ms = 100.0;

/* Flame-graph tint: an inline red background whose opacity scales with the
   span, pale (untinted) for quick and red for slow. Rather than scaling purely
   relative to `max` (which makes the slowest cell fully red even when it's
   fast), map the span into [floor, anchor] where anchor = max(max, ceiling):
   below the floor is untinted, and the red end holds at a fixed frame-time
   ceiling until something exceeds it, then stretches to it. An alpha overlay
   (not a solid color) keeps text legible in both light and dark themes. */
let heat_style = (~max: Core.Time_ns.Span.t, s: Core.Time_ns.Span.t): string => {
  let m = Core.Time_ns.Span.to_ms(max);
  let v = Core.Time_ns.Span.to_ms(s);
  let anchor = m > heat_ceil_ms ? m : heat_ceil_ms;
  let frac = (v -. heat_floor_ms) /. (anchor -. heat_floor_ms);
  let frac = frac < 0.0 ? 0.0 : frac > 1.0 ? 1.0 : frac;
  Printf.sprintf("background-color: rgba(210, 45, 45, %.3f)", frac *. 0.8);
};

/* --- cells --- */

let text_cell = (s: string): Node.t => Node.td([text(s)]);

let int_cell = (n: int): Node.t => text_cell(string_of_int(n));

/* Truncate a possibly-long label; the full text goes on the cell's tooltip. */
let truncate = (n: int, s: string): string =>
  String.length(s) <= n ? s : String.sub(s, 0, n) ++ {|…|};

/* A left-aligned name (an encoding, a mode) rather than a right-aligned
   number. */
let name_cell = (s: string): Node.t =>
  Node.td(~attrs=[clss(["perf-name"])], [text(s)]);

/* A left-aligned, truncated label (an edit action) with the full text on its
   tooltip. */
let label_cell = (s: string): Node.t =>
  Node.td(
    ~attrs=[clss(["perf-action"]), Attr.title(s)],
    [text(truncate(16, s))],
  );

/* An outcome in its own color — `cls` picks it — with `tooltip` explaining it
   when there is more to say than the label (e.g. a failure message). */
let status_cell =
    (~cls: string, ~tooltip: option(string)=None, s: string): Node.t =>
  Node.td(
    ~attrs=
      [clss([cls])]
      @ (
        switch (tooltip) {
        | None => []
        | Some(t) => [Attr.title(t)]
        }
      ),
    [text(s)],
  );

/* A timing tinted by its duration relative to `max`. `total` renders the bold
   summary column. */
let heat_cell =
    (
      ~max: Core.Time_ns.Span.t,
      ~total: bool=false,
      s: option(Core.Time_ns.Span.t),
    )
    : Node.t => {
  let cls = total ? ["perf-total"] : [];
  switch (s) {
  | None => Node.td(~attrs=[clss(cls)], [text(fmt_opt(fmt_span, s))])
  | Some(span) =>
    Node.td(
      ~attrs=[clss(cls), Attr.create("style", heat_style(~max, span))],
      [text(fmt_span(span))],
    )
  };
};

/* --- tables --- */

/* One column: its header, the hover text explaining what it measures, and how
   to render a row's cell. */
type column('row) = {
  label: string,
  tooltip: string,
  cell: 'row => Node.t,
};

/* A full-width label row separating groups of data rows. */
type group = {
  cls: string,
  label: string,
};

type row('data) =
  | Row('data)
  | Group(group);

let head = (c: column('row)): Node.t =>
  Node.td(
    ~attrs=[clss(["perf-head"]), Attr.title(c.tooltip)],
    [text(c.label)],
  );

/* Tooltipped header row plus the rows, in a horizontal scroller (columns
   overflow the narrow sidebar). A group label spans every column, so its
   colspan follows the column list rather than a hardcoded count. */
let table =
    (~columns: list(column('data)), rows: list(row('data))): Node.t => {
  let width = string_of_int(List.length(columns));
  let node_of_row = (r: row('data)): Node.t =>
    switch (r) {
    | Row(data) => Node.tr(List.map(c => c.cell(data), columns))
    | Group({cls, label}) =>
      Node.tr([
        Node.td(
          ~attrs=[Attr.create("colspan", width), clss(["perf-group", cls])],
          [text(label)],
        ),
      ])
    };
  div(
    ~attrs=[clss(["perf-scroll"])],
    [
      Node.table(
        ~attrs=[clss(["perf-table"])],
        [Node.tr(List.map(head, columns)), ...List.map(node_of_row, rows)],
      ),
    ],
  );
};

let empty = (msg: string): Node.t =>
  div(~attrs=[clss(["perf-empty"])], [text(msg)]);

let note = (msg: string): Node.t =>
  div(~attrs=[clss(["perf-note"])], [text(msg)]);
