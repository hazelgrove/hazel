open Haz3lcore;
open Example;
open ExplainThisForm;

let _typ_elem = typ("ty_elem");
// TODO Syntactic form coloring looks off for this one and other types ones...
let list_typ_coloring_ids = (~elem_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_elem), elem_id),
];
let list_typ: form = {
  let explanation = "List type. The list type classifies lists with elements with the corresponding [*element type*](%i).";
  {
    id: ListTyp,
    syntactic_form: [mk_list_typ([[_typ_elem]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let list: group = {id: ListTyp, forms: [list_typ]};
