[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t;

let root: t;
// let compare: (t, t) => int;
// let lca: (t, t) => t;

let to_str: t => string;
// hack to do sort-specific stuff in web
// todo: unhack
let of_str: string => t;

module Set: Set.S with type elt = t;
module Map: Map.S with type key = t;
