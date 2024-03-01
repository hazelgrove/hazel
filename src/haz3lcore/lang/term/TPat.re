[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Var;

include TermBase.TPat;

let rep_id = ({ids, _}) => {
  assert(ids != []);
  List.hd(ids);
};

let hole = (tms: list(TermBase.Any.t)) =>
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
