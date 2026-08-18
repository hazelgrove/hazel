open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* Shared formatting and table building for the instrumented debug sections
   (Worker Messaging, Evaluation, Statics, Editor & Memory, Frame Timing): an em
   dash for a metric that didn't run, durations via Core.Time_ns.Span, sizes via
   Core.Byte_units, and the flame-graph heat map.

   A table is described as data, not built as markup. A `column` carries its
   header, the tooltip explaining what it measures, and a projection to a `cell`
   — and `cell` is opaque, so a section says *what* a cell shows and this file
   alone decides how it is drawn. Nothing outside can style a cell, name a CSS
   class, or tint against a different scale than the table it sits in. */

/* What a metric that didn't run reads as, rather than a misleading 0. */
let dash = {|—|};

let fmt_span = (s: Core.Time_ns.Span.t): string =>
  Core.Time_ns.Span.to_string_hum(~decimals=2, s);

let fmt_bytes = (b: Core.Byte_units.t): string =>
  Core.Byte_units.to_string_hum(b);

/* --- what a cell shows --- */

/* How an outcome reads, which is what picks its color: a section reports the
   outcome and never names a class. */
type outcome =
  | Good
  | Bad
  | Waiting;

/* A cell description. Deliberately not a Node.t: see the header comment. */
type cell =
  | Text(string)
  | Int(int)
  | Name(string)
  | Label(string)
  | Status(outcome, option(string), string)
  | Heat(option(Core.Time_ns.Span.t))
  | HeatTotal(option(Core.Time_ns.Span.t));

let text_cell = (s: string): cell => Text(s);

let int_cell = (n: int): cell => Int(n);

let bytes_cell = (b: Core.Byte_units.t): cell => Text(fmt_bytes(b));

/* A metric that didn't run: an em dash in place of the cell it would have been.
   Sections hand over `option(cell)` and never spell the absence themselves. */
let opt_cell = (c: option(cell)): cell =>
  Option.value(c, ~default=Text(dash));

/* A left-aligned name (an encoding, a mode) rather than a right-aligned
   number. */
let name_cell = (s: string): cell => Name(s);

/* A left-aligned edit-action label; it is truncated on the way out, with the
   full text on the cell's tooltip. */
let label_cell = (s: string): cell => Label(s);

/* An outcome in its own color, with `tooltip` when there is more to say than the
   label (e.g. a failure message). */
let status_cell =
    (~outcome: outcome, ~tooltip: option(string)=None, s: string): cell =>
  Status(outcome, tooltip, s);

/* A timing, tinted by its duration relative to the table's heat scale. */
let heat_cell = (s: option(Core.Time_ns.Span.t)): cell => Heat(s);

/* The bold summary timing of a row. Every table here has one, and it dominates
   its own row — which is what makes the derived heat scale below correct. */
let total_cell = (s: option(Core.Time_ns.Span.t)): cell => HeatTotal(s);

/* --- columns and rows --- */

/* One column: its header, the hover text explaining what it measures, and how
   to describe a row's cell. */
type column('row) = {
  label: string,
  tooltip: string,
  cell: 'row => cell,
};

/* A full-width label row separating groups of data rows. */
type group_kind =
  | Primary /* starts a new group: top rule + tint */
  | Secondary /* a lighter sub-header inside the group */
  | Absent; /* italic note that the group has nothing to show */

type group = {
  kind: group_kind,
  label: string,
};

type row('data) =
  | Row('data)
  | Group(group);

/* The edit-action column all three per-frame tables open with, so its wording
   lives in one place rather than once per section. `get` pulls the row's action
   label out; the column owns how it is described and rendered. */
let action_column = (get: 'row => option(string)): column('row) => {
  label: "action",
  tooltip: "The edit action that triggered this frame.",
  cell: r => opt_cell(Option.map(label_cell, get(r))),
};

/* --- the heat scale --- */

/* The span a cell contributes to the scale, if any. */
let heat_span = (c: cell): option(Core.Time_ns.Span.t) =>
  switch (c) {
  | Heat(s)
  | HeatTotal(s) => s
  | Text(_)
  | Int(_)
  | Name(_)
  | Label(_)
  | Status(_) => None
  };

/* The heat scale a table tints against: the peak timing among its own cells.
   Derived rather than passed, so a section cannot tint against one scale and
   describe another in its legend — `table` computes this too, from the same
   rows. Timings that didn't run drop out, so the fold is just `max`.

   This is the same value the sections used to compute by hand, because every
   table here carries a total column that dominates its own row: a frame's total
   covers its stages, an encoding's total is the sum of its three phases, and a
   request's round trip contains its evaluation. */
let scale =
    (~columns: list(column('data)), rows: list(row('data)))
    : Core.Time_ns.Span.t =>
  rows
  |> List.to_seq
  |> Seq.concat_map((r: row('data)) =>
       switch (r) {
       | Group(_) => Seq.empty
       | Row(data) => columns |> List.to_seq |> Seq.map(c => c.cell(data))
       }
     )
  |> Seq.filter_map(heat_span)
  |> Seq.fold_left(Core.Time_ns.Span.max, Core.Time_ns.Span.zero);

/* --- rendering: the only part that knows about markup --- */

/* Below this a timing reads as instant (no tint), so fast frames stay pale no
   matter how they compare to each other. */
let heat_floor_ms = 0.5;
/* The red end of the scale never anchors below this, so as long as everything
   is under ~100ms nothing goes deep red; once something is slower, the anchor
   stretches out to the visible max. */
let heat_ceil_ms = 100.0;

/* Flame-graph tint: an inline red background whose opacity scales with the
   span, pale (untinted) for quick and red for slow. Rather than scaling purely
   relative to `scale` (which makes the slowest cell fully red even when it's
   fast), map the span into [floor, anchor] where anchor = max(scale, ceiling):
   below the floor is untinted, and the red end holds at a fixed frame-time
   ceiling until something exceeds it, then stretches to it. An alpha overlay
   (not a solid color) keeps text legible in both light and dark themes. */
let heat_style = (~scale: Core.Time_ns.Span.t, s: Core.Time_ns.Span.t): string => {
  let m = Core.Time_ns.Span.to_ms(scale);
  let v = Core.Time_ns.Span.to_ms(s);
  let anchor = m > heat_ceil_ms ? m : heat_ceil_ms;
  let frac = (v -. heat_floor_ms) /. (anchor -. heat_floor_ms);
  let frac = frac < 0.0 ? 0.0 : frac > 1.0 ? 1.0 : frac;
  Printf.sprintf("background-color: rgba(210, 45, 45, %.3f)", frac *. 0.8);
};

/* Truncate a possibly-long label; the full text goes on the cell's tooltip. */
let truncate = (n: int, s: string): string =>
  String.length(s) <= n ? s : String.sub(s, 0, n) ++ {|…|};

let outcome_cls = (o: outcome): string =>
  switch (o) {
  | Good => "perf-ok"
  | Bad => "perf-fail"
  | Waiting => "perf-pending"
  };

let group_cls = (k: group_kind): string =>
  switch (k) {
  | Primary => "perf-group-primary"
  | Secondary => "perf-group-secondary"
  | Absent => "perf-group-absent"
  };

let title_attrs = (tooltip: option(string)): list(Attr.t) =>
  tooltip |> Option.map(Attr.title) |> Option.to_list;

let heat_td =
    (
      ~scale: Core.Time_ns.Span.t,
      ~cls: list(string),
      s: option(Core.Time_ns.Span.t),
    )
    : Node.t =>
  switch (s) {
  | None => Node.td(~attrs=[clss(cls)], [text(dash)])
  | Some(span) =>
    Node.td(
      ~attrs=[clss(cls), Attr.create("style", heat_style(~scale, span))],
      [text(fmt_span(span))],
    )
  };

let node_of_cell = (~scale: Core.Time_ns.Span.t, c: cell): Node.t =>
  switch (c) {
  | Text(s) => Node.td([text(s)])
  | Int(n) => Node.td([text(string_of_int(n))])
  | Name(s) => Node.td(~attrs=[clss(["perf-name"])], [text(s)])
  | Label(s) =>
    Node.td(
      ~attrs=[clss(["perf-action"]), Attr.title(s)],
      [text(truncate(16, s))],
    )
  | Status(outcome, tooltip, s) =>
    Node.td(
      ~attrs=[clss([outcome_cls(outcome)]), ...title_attrs(tooltip)],
      [text(s)],
    )
  | Heat(s) => heat_td(~scale, ~cls=[], s)
  | HeatTotal(s) => heat_td(~scale, ~cls=["perf-total"], s)
  };

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
  let scale = scale(~columns, rows);
  let width = string_of_int(List.length(columns));
  let node_of_row = (r: row('data)): Node.t =>
    switch (r) {
    | Row(data) =>
      Node.tr(List.map(c => node_of_cell(~scale, c.cell(data)), columns))
    | Group({kind, label}) =>
      Node.tr([
        Node.td(
          ~attrs=[
            Attr.create("colspan", width),
            clss(["perf-group", group_cls(kind)]),
          ],
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

/* The legend the heat tables carry: the scale the reddest cell stands for. Owned
   here so a section neither formats the span nor repeats the wording. */
let scale_note =
    (~columns: list(column('data)), rows: list(row('data))): Node.t =>
  note(
    "max total: " ++ fmt_span(scale(~columns, rows)) ++ " · redder = slower",
  );
