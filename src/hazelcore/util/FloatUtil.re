/**
 * Adds a trailing zero to OCaml's string representation.
 * Necessary for CSS property values, where it's expected
 * that there is a digit trailing decimal points.
 */
let to_string_zero = (f: float) => string_of_float(f) ++ "0";
