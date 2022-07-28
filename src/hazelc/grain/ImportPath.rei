[@deriving sexp]
type t;

let get: t => [> | `Lib(Path.t) | `Rel(Path.t)];

let of_path: Path.t => t;
let of_string: string => t;

let to_path: t => Path.t;
let to_string: t => string;

let is_lib: t => bool;
let is_rel: t => bool;
