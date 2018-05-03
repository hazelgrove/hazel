open Semantics.Core;


/**
 * Outputs the passed UHExp to the passed formatter as concrete syntax
 * (defined in HZParse.mly and HZLex.mll).
 * If the passed UHExp doesn't type check, or is otherwise not well formed,
 * IllFormed will be raised.
 */
let hz_serialize: Format.formatter => UHExp.t => unit;

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
