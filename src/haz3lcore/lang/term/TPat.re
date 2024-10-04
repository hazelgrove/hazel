[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Var;

include TermBase.TPat;

let rep_id: t => Id.t = IdTagged.rep_id;
let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.TPat.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: term => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Var(_) => Var;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type alias"
  | MultiHole => "Broken type alias"
  | EmptyHole => "Empty type alias hole"
  | Var => "Type alias";
