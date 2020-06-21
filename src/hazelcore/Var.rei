[@deriving sexp]
type t = string;

let eq: (String.t, String.t) => bool;

let length: string => int;

let valid_regex: Re.Str.regexp;

let is_valid: string => bool;

let check_valid: (string, option('a)) => option('a);

let is_true: String.t => bool;

let is_false: String.t => bool;

let is_let: String.t => bool;

let is_case: String.t => bool;

let is_wild: String.t => bool;

let split: (int, string) => (string, string);
