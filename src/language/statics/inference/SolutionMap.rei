include (module type of ProvMap);
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = ProvMap.t('a);

let lookup_prov: (Prov.t, t('a)) => option('a);
// let replace_cycles: (t('a), list(StringProv.t)) => t('a);
