open Sexplib.Std;

[@deriving sexp]
type steps = list(ChildIndex.t);

[@deriving sexp]
type t = (steps, CursorPosition.t);

[@deriving sexp]
type rev_steps = steps;

[@deriving sexp]
type rev_t = (CursorPosition.t, rev_steps);

let rev = ((cursor, rev_steps): rev_t): t => (
  rev_steps |> List.rev,
  cursor,
);

let append = (steps, (appendee_steps, appendee_cursor): t): t => (
  steps @ appendee_steps,
  appendee_cursor,
);

[@deriving sexp]
type hole_shape =
  | TypeErr
  | VarErr
  | Empty;

[@deriving sexp]
type hole_sort =
  | TypHole
  | PatHole(MetaVar.t, hole_shape)
  | ExpHole(MetaVar.t, hole_shape)
  | LivelitHole(MetaVar.t)
  | ApLivelit(MetaVar.t);

[@deriving sexp]
type hole_info = {
  sort: hole_sort,
  steps,
};

[@deriving sexp]
type hole_list = list(hole_info);

/* two hole lists, one for before the cursor, one for after */
[@deriving sexp]
type zhole_list = {
  holes_before: hole_list,
  hole_selected: option(hole_info),
  holes_after: hole_list,
};
