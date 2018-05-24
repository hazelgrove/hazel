open Semantics.Core;


/**
 * Outputs the passed UHExp to the passed formatter as concrete syntax
 * (defined in HZParse.mly and HZLex.mll).
 * fmtr is the formatter to output to. Defaults to standard output.
 * line_length is the maximum desired length of a serialized line. Note that
 * it's possible for a line to exceed this length. Defaults to 100.
 * indent is the standard indentation width. Defaults to 2.
 * If the passed UHExp doesn't type check, or is otherwise not well formed,
 * IllFormed will be raised.
 */
let hz_serialize: fmtr::Format.formatter? => line_length::int? => indent::int? => UHExp.t => unit;

/**
 * Convenience function for hz_serialize.
 */
let string_of_uhexp: UHExp.t => string;


/**
 * Reads the passed in_channel and parses it from the concrete syntax
 * (defined in HZParse.mly and HZLex.mll) into a (UHExp, UHTyp, MetaVar).
 * If the in_channel can't be parsed correctly, InvalidSyntax will be raised.
 * If the passed UHExp doesn't type check, or is otherwise IllFormed,
 * IllFormed will be raised.
 */
let hz_parse: in_channel => (UHExp.t, HTyp.t, MetaVar.t);

/**
 * Convenience function for hz_parse.
 */
let uhexp_of_string: string => UHExp.t;
