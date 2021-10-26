[@deriving sexp]
type t;

let to_string: t => string;

let from_string: string => (t, list(StringLitLexer.error));

let equal: (t, t) => bool;

let concat: (t, t) => t;
