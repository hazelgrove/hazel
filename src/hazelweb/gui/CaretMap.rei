/**
 * Static mutable bidirectional map linking
 * (row, col) positions in the contenteditable
 * element with reversed cursor paths.
 *
 * TODO: make non-static when there is more than one cell
 */

let add: ((int, int), CursorPath.rev_t) => unit;
let lookup_rowcol: ((int, int)) => option(CursorPath.rev_t);
let lookup_rowcol_before:
  ((int, int)) => option(((int, int), CursorPath.rev_t));
let lookup_rowcol_after:
  ((int, int)) => option(((int, int), CursorPath.rev_t));
let lookup_revpath: CursorPath.rev_t => (int, int);

let anchor_of_revpath:
  CursorPath.rev_t => (Js_of_ocaml.Js.t(Js_of_ocaml.Dom.node), int);

let revpath_of_anchor: unit => CursorPath.rev_t;

/*
 let set_caret_rowcol: (~state: State.t, (int, int)) => CursorPath.rev_t;

 let set_caret_revpath: (~state: State.t, CursorPath.rev_t) => unit;
 */
