/**
 * ADT for the strings corresponding to string literal bodies,
 * which must be unescaped to be in normal form
 */
[@deriving sexp]
type t = pri string;

let to_string: t => string;

/** Similar to Scanf.unescaped except it is total, i.e. it does not fail
 *  when it sees invalid escape sequences. Instead, it leaves them in
 *  verbatim and returns a list of errors.
 *
 *  Does not currently support unicode escapes (\u{...}).
 *
 *  The following should all work:
 *
 *   unescaped("\\n\n")
 *   unescaped("x\\ay\\n")    (\a is not valid)
 *   unescaped("\\n\\)        (trailing \ is ok)
 *   unescaped("\\n\"\\n")    (unescaped quote is ok)
 *
 */
let unescaped: StringLitBody.t => (t, list(StringLitLexer.error));

let equal: (t, t) => bool;
let cat: (t, t) => t;

[@deriving sexp]
type subscript_error =
  | StartIndexOutOfBounds
  | EndIndexOutOfBounds
  | BothIndicesOutOfBounds
  | EndIndexBeforeStart;

type subscript_result =
  | Error(subscript_error)
  | OK(t);

/** Implements s[start:end) */
let subscript: (t, int, int) => subscript_result;
