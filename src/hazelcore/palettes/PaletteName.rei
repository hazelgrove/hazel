[@deriving (sexp, show)]
type t = string;

let eq: (t, t) => bool;
let valid_regex: Re.Str.regexp;
let is_valid: string => bool;
let check_valid: (string, option('a)) => option('a);
