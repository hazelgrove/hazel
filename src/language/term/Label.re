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
let ids: t => list(Id.t) = IdTagged.ids;

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

/* The label's name, if it is a well-formed one */
let name = (l: t): option(string) =>
  switch (l.term) {
  | Label(name) => Some(name)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | ExplicitNonlabel => None
  };

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
  | ExplicitNonlabel => "Explicitly unlabeled entry";

/* Same kind of label with the same text; a malformed label's pieces are
   not compared */
let equal = (l1: t, l2: t): bool =>
  equal_cls(cls_of_term(l1.term), cls_of_term(l2.term))
  && (
    switch (l1.term, l2.term) {
    | (Label(s1), Label(s2))
    | (Invalid(s1), Invalid(s2)) => String.equal(s1, s2)
    | _ => true
    }
  );
