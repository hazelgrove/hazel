include (module type of ProvMap);
[@deriving (show({with_path: false}), sexp, yojson)]
type t = ProvMap.t(Solution.t);

let lookup_prov: (Prov.t, t) => option(Solution.t);
let replace_cycles: (t, list(StringProv.t)) => t;
