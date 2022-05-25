/**
  String wrapper type where escape sequences are replaced with their actual
  characters.
 */

/**
  The type for unescaped strings.
 */
[@deriving sexp]
type t;

/**
  Convert an unescaped string to a regular string.
 */
let to_string: t => string;

/**
  Parse a string into an unescaped string.
 */
let from_string: string => (t, list(StringLitLexer.error));

/**
  Convert a string to an unescaped string without parsing.
 */
let from_string_unchecked: string => t;

/**
  Return the length of an unescaped string.
 */
let length: t => int;

/**
  Return [true] if two unescaped strings are equal.
 */
let equal: (t, t) => bool;

/**
  Concatenate two unescaped strings.
 */
let concat: (t, t) => t;

/**

 */
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

[@deriving sexp]
type subscript_result =
  | Ok(t)
  | Err(subscript_error);

/**
  Subscript an unescaped string.
 */
let subscript: (t, int, int) => subscript_result;
