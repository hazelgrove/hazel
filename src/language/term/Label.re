/* A tuple entry's label, or the field named on the right of a dot:
   the term of the Label sort. */

[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Label
  | ExplicitNonlabel;

include TermBase.Label;

let rep_id: t => Id.t = IdTagged.rep_id;

let hole = (tms: list(TermBase.Any.t)): term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.label_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Label(_) => Label
  | ExplicitNonlabel => ExplicitNonlabel;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid label"
  | EmptyHole => "Empty label hole"
  | MultiHole => "Broken label"
  | Label => "Label"
  | ExplicitNonlabel => "Explicit non-label";
