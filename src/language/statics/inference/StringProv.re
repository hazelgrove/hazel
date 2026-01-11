open Util;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = (string, Id.t);
let compare = ((k1, id1), (k2, id2)) => {
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
);
