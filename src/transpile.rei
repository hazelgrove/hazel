/**
 * raised by hz_to_ml, serialize, or hz_to_file if a hazelnut var has a name
 * that cannot be a valid ocaml value identifier
 */
exception InvalidVar string;

/**
 * Returns an ml Parsetree expression equivalent to the passed HExp.
 * The retuned expression may depend on the HazelPrelude module.
 */
let hz_to_ml: Semantics.Core.HExp.t => Parsetree.expression;
/* TODO let ml_to_hz : Parsetree.expression => Semantics.Core.HExp.t; */
let serialize: Format.formatter => Semantics.Core.HExp.t => unit;
let hz_to_string: Semantics.Core.HExp.t => string;
/* TODO let parse : ??? => Parsetree.expression => unit; */
let hz_to_file: string => Semantics.Core.HExp.t => unit;
/* TODO let hz_from_file : string => Parsetree.expression; */
