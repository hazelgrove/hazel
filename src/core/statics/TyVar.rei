[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let length: t => int;

let equal: (t, t) => bool;

let is_valid: string => bool;
let is_reserved: string => bool;
