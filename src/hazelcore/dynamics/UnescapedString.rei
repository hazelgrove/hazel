/**
 * ADT for the strings corresponding to string literal bodies,
 * which must be unescaped to be in normal form
 */
[@deriving sexp]
type t = pri string;

let to_string: t => string;

/** Same as Scanf.unescaped except it is total, i.e. it does not fail
 *  when it sees invalid escape sequences. Instead, it leaves them in
 *  verbatim.
 *
 * If Scanf.unescaped(s) = s' then unscaped_total(s) = s', e.g.
 *
 *   unescaped_total("\\n\n") = "\n\n"       (same as Scanf.unescaped)
 *
 * The following situations cause Scanf.unescaped to raise an exception,
 * but unescaped_total unescapes around them:
 *
 *   unescaped_total("x\\ay\\n") = "x\\ay\n"  (\a is not valid)
 *   unescaped_total("\\n\\") = "\n\\"        (trailing \ is ok)
 *   unescaped_total("\\n\"\\n") = "\\n\"\\n" (unescaped quote is ok)
 *
 */
let unescaped_total: string => t;

let equal: (t, t) => bool;
let cat: (t, t) => t;
