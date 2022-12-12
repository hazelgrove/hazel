open Sexplib.Std;

[@deriving sexp]
type steps = list(ChildIndex.t);

[@deriving sexp]
type t = (steps, CursorPosition.t);

[@deriving sexp]
type rev_steps = steps;

[@deriving sexp]
type rev_t = (CursorPosition.t, rev_steps);

[@deriving sexp]
type hole_shape =
  | TypeErr
  | VarErr
  | Empty;

[@deriving sexp]
type hole_sort =
  | TypHole
  | PatHole(MetaVar.t, hole_shape)
  | ExpHole(MetaVar.t, hole_shape);

[@deriving sexp]
type hole_info = {
  sort: hole_sort,
  ap_steps: steps,
  steps,
};

[@deriving sexp]
type hole_list = list(hole_info) /* two hole lists, one for before the cursor, one for after */;

[@deriving sexp]
type zhole_list = {
  holes_before: hole_list,
  hole_selected: option(hole_info),
  holes_after: hole_list,
};

let mk_hole_sort = (sort, steps): hole_info => {
  sort,
  ap_steps: steps /* if this is an ap hole, steps to function pos; otherwise == steps */,
  steps,
};

let mk_hole_sort_ap = (sort, steps, ~ap_steps): hole_info => {
  sort,
  ap_steps,
  steps,
} /* If to_fpos_for_aps is true, then the steps to application errors
   will lead to the function position as opposed to the ap itself.
   This is turned on for cursor movement to holes, as there is no
   cursor position on an ap, but off to draw error hole decorations */;

let get_steps =
    (~to_fpos_for_aps: bool=false, {steps, ap_steps, _}: hole_info) =>
  to_fpos_for_aps ? ap_steps : steps;

let get_sort = ({sort, _}: hole_info): hole_sort => sort;
