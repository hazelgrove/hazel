[@deriving sexp]
type t = string;

let eq: (String.t, String.t) => bool;

let length: string => int;

let valid_regex: Re.Str.regexp;

let is_valid: string => bool;

let check_valid: (t, option('a)) => option('a);

let is_Bool: String.t => bool;

let is_Int: String.t => bool;

let is_Float: String.t => bool;
