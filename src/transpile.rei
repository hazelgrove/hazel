/**
 * raised by hz_to_ml, serialize, or hz_to_file if a hazelnut var has a name
 * that cannot be a valid ocaml value identifier
 */
exception InvalidVar string;


/**
 * raised by ml_to_hz, parse, hz_from_file, and hz_from_string if the passed
 * serialization cannot be interpreted as a valid hazelnut expression. Note
 * that even if the serialization is syntactically valid, if the resulting
 * Hazelnut expression doesn't type-check, this exception will still be raised.
 */
exception InvalidSerialization string;


/**
 * Returns an ml Parsetree expression equivalent to the passed HExp.
 * The retuned expression may depend on the HazelPrelude module.
 */
let hz_to_ml: Semantics.Core.HExp.t => Parsetree.expression;

let serialize: Format.formatter => Semantics.Core.HExp.t => unit;

let hz_to_file: string => Semantics.Core.HExp.t => unit;

let hz_to_string: Semantics.Core.HExp.t => string;


/**
 * Returns an HExp derived from the passed Parsetree.
 * The passed Parsetree may depend on the HazelPrelude module.
 * The resulting HExp must type check or InvalidSerialization will be raised.
 */
let ml_to_hz: Parsetree.expression => Semantics.Core.HExp.t;

let parse: in_channel => Semantics.Core.HExp.t;

let hz_from_file: string => Semantics.Core.HExp.t;

let hz_from_string: string => Semantics.Core.HExp.t;
