open Semantics.Core;


/**
 * Outputs the passed UHExp to the passed formatter as concrete syntax
 * (defined in HazelParse.mly and HazelLex.mll).
 * fmtr is the formatter to output to. Defaults to standard output.
 * line_length is the maximum desired length of a serialized line. Note that
 * it's possible for a line to exceed this length. Defaults to 100.
 * indent is the standard indentation width. Defaults to 2.
 * If the passed UHExp doesn't type check, or is otherwise not well formed,
 * LangUtil.IllFormed will be raised.
 */
let serialize: fmtr::Format.formatter? => line_length::int? => indent::int? => UHExp.t => unit;

/**
 * Convenience function for serialize.
 */
let string_of_uhexp: UHExp.t => string;
