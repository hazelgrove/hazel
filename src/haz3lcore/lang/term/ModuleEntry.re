[@deriving (show({with_path: false}), sexp, yojson)]
type t = TermBase.module_entry_t;

let rep_id = ({annotation: {ids, _}, _}: t) =>
  switch (ids) {
  | [] => raise(Invalid_argument("Exp.rep_id"))
  | [id, ..._] => id
  };
