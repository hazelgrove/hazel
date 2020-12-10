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

let rec compare_steps = (steps, steps') =>
  switch (steps, steps') {
  | ([], []) => 0
  | ([], [_, ..._]) => (-1)
  | ([_, ..._], []) => 1
  | ([step, ...steps], [step', ...steps']) =>
    let c = Int.compare(step, step');
    c == 0 ? compare_steps(steps, steps') : c;
  };

let compare = ((steps, j), (steps', j')) => {
  let c = compare_steps(steps, steps');
  c == 0 ? Int.compare(j, j') : c;
};
