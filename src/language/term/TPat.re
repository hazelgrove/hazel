[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Var
  | Param;

include TermBase.TPat;

let fast_equal = Equality.syntactic.tpat;
let equal = fast_equal;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.TPat.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.tpat_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Var(_) => Var
  | Param(_) => Param;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type alias"
  | MultiHole => "Broken type alias"
  | EmptyHole => "Type alias hole"
  | Var => "Type alias"
  | Param => "Parameterized type alias";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

let alias_head = (tpat: t): option((string, list(t))) =>
  switch (tpat.term) {
  | Var(name) => Some((name, []))
  | Param(name, params) => Some((name, params))
  | _ => None
  };
