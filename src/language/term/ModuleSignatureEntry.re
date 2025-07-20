[@deriving (show({with_path: false}), sexp, yojson)]
type t = TermBase.module_signature_entry_t;
type term = TermBase.module_signature_entry_term;
let term_of: t => term = IdTagged.term_of;

let rep_id = ({annotation: {ids, _}, _}: t) =>
  switch (ids) {
  | [] => raise(Invalid_argument("Exp.rep_id"))
  | [id, ..._] => id
  };
