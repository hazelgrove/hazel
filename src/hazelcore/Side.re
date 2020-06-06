[@deriving sexp]
type t =
  | Before
  | After;

let toggle =
  fun
  | Before => After
  | After => Before;

let to_string =
  fun
  | Before => "Before"
  | After => "After";
