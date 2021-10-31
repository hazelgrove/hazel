[@deriving sexp]
type t;

let to_string: t => string;

let from_string: string => (t, list(StringLitLexer.error));

let length: t => int;

let equal: (t, t) => bool;

let concat: (t, t) => t;

[@deriving sexp]
type out_of_bounds_error = {
  idx: int,
  lower: int,
  upper: int,
};

[@deriving sexp]
type subscript_error =
  | StartIndexOutOfBounds(out_of_bounds_error)
  | EndIndexOutOfBounds(out_of_bounds_error)
  | BothIndicesOutOfBounds(out_of_bounds_error, out_of_bounds_error)
  | EndIndexBeforeStart(out_of_bounds_error)
  | EmptyString;

type subscript_result =
  | Ok(t)
  | Err(subscript_error);

let subscript: (t, int, int) => subscript_result;
