[@deriving sexp]
type t =
  | Before
  | After;

let toggle =
  fun
  | Before => After
  | After => Before;
