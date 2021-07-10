[@deriving sexp]
type t;

let of_string: string => t;
let to_string: t => string;

let eq: (t, t) => bool;

let length: t => int;

let valid_regex: Re.Str.regexp;

let is_valid: t => bool;

let is_Bool: t => bool;

let is_Int: t => bool;

let is_Float: t => bool;
