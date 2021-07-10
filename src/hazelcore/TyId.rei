[@deriving sexp]
type t;

/// Needed to avoid a circular dependency with HTyp
module BuiltInType: {
  [@deriving sexp]
  type t =
    | Bool
    | Float
    | Int;
};

let of_string: string => t;
let to_string: t => string;

let eq: (t, t) => bool;

let length: t => int;

let valid_regex: Re.Str.regexp;

let is_valid: t => bool;

let is_Bool: t => bool;

let is_Int: t => bool;

let is_Float: t => bool;

let to_builtin_type: t => option(BuiltInType.t);
