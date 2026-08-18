/* Shared formatting and table building for the instrumented debug sections; see
 * PerfFormat.re. Sections describe a table as columns + rows and never build
 * HTML themselves. */

/* Renderings of the units the panels report; `fmt_opt` gives an em dash for a
 * metric that didn't run rather than a misleading 0. */
let fmt_opt: ('a => string, option('a)) => string;
let fmt_span: Core.Time_ns.Span.t => string;
let fmt_bytes: Core.Byte_units.t => string;

/* Largest span across a column, for the heat-map scale. */
let max_span: list(option(Core.Time_ns.Span.t)) => Core.Time_ns.Span.t;

/* Cell builders, for a column's `cell`. */
let text_cell: string => Util.WebUtil.Node.t;
let int_cell: int => Util.WebUtil.Node.t;
let name_cell: string => Util.WebUtil.Node.t;
let label_cell: string => Util.WebUtil.Node.t;
let status_cell:
  (~cls: string, ~tooltip: option(string)=?, string) => Util.WebUtil.Node.t;
let heat_cell:
  (~max: Core.Time_ns.Span.t, ~total: bool=?, option(Core.Time_ns.Span.t)) =>
  Util.WebUtil.Node.t;

/* One column: its header, the tooltip explaining what it measures, and how to
 * render a row's cell — one value, so header and cells cannot drift apart. */
type column('row) = {
  label: string,
  tooltip: string,
  cell: 'row => Util.WebUtil.Node.t,
};

/* A full-width label row separating groups of data rows. */
type group = {
  cls: string,
  label: string,
};

type row('data) =
  | Row('data)
  | Group(group);

/* The shared edit-action column; `get` pulls the row's action label out. */
let action_column: ('row => string) => column('row);

let table:
  (~columns: list(column('data)), list(row('data))) => Util.WebUtil.Node.t;

/* Italic one-liners above a table. */
let empty: string => Util.WebUtil.Node.t;
let note: string => Util.WebUtil.Node.t;
