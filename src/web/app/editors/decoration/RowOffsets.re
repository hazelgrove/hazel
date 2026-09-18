/* Per-row end-of-line occupancy shared by offside decorations, so
   they stack instead of overlapping: the quiver publishes the right
   edge of its boxes (quiver renders first), and the probe offside
   display starts after. Columns from row start, not pixels.

   Reset unconditionally at the start of each editable-editor render
   (CodeEditable) — claims only ever come from that same render pass,
   which builds sequentially, so a plain mutable table suffices
   (precedent: Code.MkDeferredLinebreaks). Assumes offside consumers
   render within the same editor pass as the reset. */

let table: Hashtbl.t(int, int) = Hashtbl.create(16);

let reset = (): unit => Hashtbl.reset(table);

let claim = (~row: int, ~until_col: int): unit => {
  let prev =
    switch (Hashtbl.find_opt(table, row)) {
    | Some(c) => c
    | None => 0
    };
  Hashtbl.replace(table, row, max(prev, until_col));
};

let claimed = (~row: int): int =>
  switch (Hashtbl.find_opt(table, row)) {
  | Some(c) => c
  | None => 0
  };
