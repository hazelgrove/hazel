open Semantics.Core;

/**
 * Reads the passed in_channel and deserializes it from the concrete syntax
 * (defined in HazelParse.mly and HazelLex.mll) into a (UHExp, UHTyp, MetaVar).
 * If the in_channel can't be parsed correctly, LangUtil.InvalidSyntax will be
 * raised.
 * If the passed UHExp doesn't type check, or is otherwise ill-formed,
 * LangUtil.IllFormed will be raised.
 */
let deserialize: in_channel => (UHExp.t, HTyp.t, MetaVar.t);

/**
 * Convenience function for deserialize.
 */
let uhexp_of_string: string => UHExp.t;
