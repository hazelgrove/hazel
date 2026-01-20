open Util;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = (string, Id.t, Prov.t);
let compare = ((k1, id1, _): t, (k2, id2, _): t) => {
  let id_compare = Id.compare(id1, id2);
  if (id_compare != 0) {
    id_compare;
  } else {
    String.compare(k1, k2);
  };
};

let of_prov = (p: Prov.t): t => (
  Prov.to_string(Prov.term_of(p)),
  IdTagged.rep_id(p),
  p,
);

let to_prov = ((_, _, prov): t) => prov;
