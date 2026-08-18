/* Shared table building for the instrumented debug sections; see PerfFormat.re.
 * A section describes its table as columns and rows and never builds markup or
 * formats a value: `cell` is opaque, so how a cell is drawn — its classes, its
 * tint, its units, the em dash for a metric that didn't run — is decided here
 * and nowhere else. */

/* How an outcome reads, which is what picks its color. */
type outcome =
  | Good
  | Bad
  | Waiting;

/* What a cell shows. Opaque: build one with the constructors below. */
type cell;

let text_cell: string => cell;
let int_cell: int => cell;
let bytes_cell: Core.Byte_units.t => cell;
let name_cell: string => cell;
let label_cell: string => cell;
let status_cell:
  (~outcome: outcome, ~tooltip: option(string)=?, string) => cell;
let heat_cell: option(Core.Time_ns.Span.t) => cell;
let total_cell: option(Core.Time_ns.Span.t) => cell;

/* A metric that didn't run reads as an em dash. */
let opt_cell: option(cell) => cell;

/* One column: its header, the tooltip explaining what it measures, and how to
 * describe a row's cell — one value, so header and cells cannot drift apart. */
type column('row) = {
  label: string,
  tooltip: string,
  cell: 'row => cell,
};

/* The shared edit-action column; `get` pulls the row's action label out, if it
 * has one. */
let action_column: ('row => option(string)) => column('row);

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

let table:
  (~columns: list(column('data)), list(row('data))) => Util.WebUtil.Node.t;

/* The legend for a table's heat map, naming the scale its own cells set. */
let scale_note:
  (~columns: list(column('data)), list(row('data))) => Util.WebUtil.Node.t;

/* Italic one-liners above a table. */
let empty: string => Util.WebUtil.Node.t;
let note: string => Util.WebUtil.Node.t;
