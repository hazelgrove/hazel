open Haz3lcore;
open Example;
open ExplainThisForm;

let typ_elem = typ("ty_elem");
// TODO Syntactic form coloring looks off for this one and other types ones...
let list_typ_coloring_ids = (~elem_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(typ_elem), elem_id),
];
let list_typ_form = [mk_list_typ([[typ_elem]])];
let list_typ = (~elem_id: Id.t): form => {
  id: ListTyp,
  syntactic_form: list_typ_form,
  colorings: list_typ_coloring_ids(~elem_id),
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "The list type classifies lists with elements with the corresponding [*element type*](%s).",
      Id.to_string(elem_id),
    ),
  examples: [],
};

let list = (~elem_id: Id.t): group => singleton(list_typ(~elem_id));
