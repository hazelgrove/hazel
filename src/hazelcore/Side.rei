[@deriving sexp]
type t =
  | Before
  | After;

let toggle: t => t;
