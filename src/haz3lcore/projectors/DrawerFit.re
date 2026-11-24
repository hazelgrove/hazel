open Util;

/* Drawers whose content the web layer renders (a probe's stepper, a
   theorem's proof), by drawer id. Each reserves the larger of the rows
   counted from its model and the rows it measured once rendered, which
   catch what the count misses, so it never runs into the code below. The
   result lives in the zipper (Refractors.drawer_rows). */

let rows = (table: Id.Map.t(int), id: Id.t): int =>
  Id.Map.find_opt(id, table) |> Option.value(~default=1);

/* a drawer opens to its whole length below its line */
let shape = (table: Id.Map.t(int), id: Id.t): ProjectorShape.t => {
  horizontal: 0,
  vertical: Tab(rows(table, id)),
};

/* the rows each counted drawer reserves */
let fit =
    (~counted: Id.Map.t(int), ~measured: Id.Map.t(int)): Id.Map.t(int) =>
  Id.Map.mapi((id, n) => max(n, rows(measured, id)), counted);
