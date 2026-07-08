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

/* A two-column "label : value" row. */
let kv = (label: string, value: string): Node.t =>
  Node.tr(
    ~attrs=[clss(["perf-kv"])],
    [
      Node.td(~attrs=[clss(["perf-k"])], [text(label)]),
      Node.td(~attrs=[clss(["perf-v"])], [text(value)]),
    ],
  );

/* A header cell. */
let head = (label: string): Node.t =>
  Node.td(~attrs=[clss(["perf-head"])], [text(label)]);

let head_row = (labels: list(string)): Node.t =>
  Node.tr(List.map(head, labels));

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

let span_cell = (s: option(Core.Time_ns.Span.t)): Node.t =>
  Node.td([text(span_str(s))]);

let int_cell = (n: int): Node.t => Node.td([text(string_of_int(n))]);

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
