[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t;

let root: t;
// let compare: (t, t) => int;
// let lca: (t, t) => t;

let to_string: t => string;
// hack to do sort-specific stuff in web
// todo: unhack
let of_string: string => t;

module Map: Map.S with type key = t;
