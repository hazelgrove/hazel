[@deriving sexp]
type t;

let of_fpath: Fpath.t => t;
let to_fpath: t => Fpath.t;

let v: string => t;
let to_string: t => string;
let of_string: string => result(t, string);

let segs: t => list(string);

let is_rel: t => bool;
let is_abs: t => bool;

let add_seg: (t, string) => t;
let append: (t, t) => t;

let add_ext: (string, t) => t;

let split_base: t => (t, t);

let relativize: (~root: t, t) => option(t);
let strip_root: t => option(t);
let strip_root': t => t;
