/**
 * String wrapper type where escape sequences are replaced with their actual
 * characters.
 */

/**
 * The type for unescaped strings.
 */
[@deriving sexp]
type t;

/**
 * Convert an unescaped string to a regular string.
 */
let to_string: t => string;

[@deriving sexp]
type seq = StringLitLexer.seq;

[@deriving sexp]
type error = StringLitLexer.error;

/**
 * Parse a string into an unescaped string.
 */
let from_string: string => (t, list(seq), list(error));

/**
 * Convert a string to an unescaped string without parsing.
 */
let from_string_unchecked: string => t;

/**
 * [length s] is the length of [s].
 */
let length: t => int;

/**
 * [equal s0 s1] is [true] if and only if [s0] and [s1] are character-wise
 * equal.
 */
let equal: (t, t) => bool;

/**
 * String concatenation.
 */
let concat: (t, t) => t;

/**
 * Out of bounds access error.
 */
[@deriving sexp]
type out_of_bounds_error = {
  idx: int,
  lower: int,
  upper: int,
};

/**
 * Subscript operation error.
 */
[@deriving sexp]
type subscript_error =
  | StartIndexOutOfBounds(out_of_bounds_error)
  | EndIndexOutOfBounds(out_of_bounds_error)
  | BothIndicesOutOfBounds(out_of_bounds_error, out_of_bounds_error)
  | EndIndexBeforeStart(out_of_bounds_error)
  | /** Cannot subscript an empty string. */
    EmptyString;

[@deriving sexp]
type subscript_result =
  | Ok(t)
  | Err(subscript_error);

/**
 * [subscript s n1 n2] is the substring of [s] that starts at position [n1] and
 * ends at position [n2 - 1].
 *
 * If [n1] and [n2] do not designate a valid substring of [s], returns a
 * [subscript_error].
 */
let subscript: (t, int, int) => subscript_result;
