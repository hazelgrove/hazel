/* Shared formatting and table building for the instrumented debug sections; see
 * PerfFormat.re. A section describes its table as columns and rows and never
 * builds markup: `cell` is opaque, so how a cell is drawn — its classes, its
 * tint, its heat scale — is decided here and nowhere else. */

/* Renderings of the units the panels report; `fmt_opt` gives an em dash for a
 * metric that didn't run rather than a misleading 0. */
let fmt_opt: ('a => string, option('a)) => string;
let fmt_span: Core.Time_ns.Span.t => string;
let fmt_bytes: Core.Byte_units.t => string;

/* How an outcome reads, which is what picks its color. */
type outcome =
  | Good
  | Bad
  | Waiting;

/* What a cell shows. Opaque: build one with the constructors below. */
type cell;

let text_cell: string => cell;
let int_cell: int => cell;
let name_cell: string => cell;
let label_cell: string => cell;
let status_cell:
  (~outcome: outcome, ~tooltip: option(string)=?, string) => cell;
let heat_cell: option(Core.Time_ns.Span.t) => cell;
let total_cell: option(Core.Time_ns.Span.t) => cell;

/* One column: its header, the tooltip explaining what it measures, and how to
 * describe a row's cell — one value, so header and cells cannot drift apart. */
type column('row) = {
  label: string,
  tooltip: string,
  cell: 'row => cell,
};

/* The shared edit-action column; `get` pulls the row's action label out. */
let action_column: ('row => string) => column('row);

/* A full-width label row separating groups of data rows. */
type group_kind =
  | Primary
  | Secondary
  | Absent;

type group = {
  kind: group_kind,
  label: string,
};

type row('data) =
  | Row('data)
  | Group(group);

/* The heat scale a table will tint against, for a section that names it in a
 * legend. `table` derives its own from the same rows, so the two agree. */
let scale:
  (~columns: list(column('data)), list(row('data))) => Core.Time_ns.Span.t;

let table:
  (~columns: list(column('data)), list(row('data))) => Util.WebUtil.Node.t;

/* Italic one-liners above a table. */
let empty: string => Util.WebUtil.Node.t;
let note: string => Util.WebUtil.Node.t;
