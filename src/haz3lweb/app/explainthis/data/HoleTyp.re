open Example;
open ExplainThisForm;
open Haz3lcore;

let empty_hole_typ: form = {
  let explanation = "This marks a type that needs to be filled in.";
  {
    id: EmptyHoleTyp,
    syntactic_form: [Grout({id: Id.mk(), shape: Convex})],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

// TODO Did get a case where in type position had space between two variables where got into weird state
let multi_hole_typ: form = {
  let explanation = "This is an invalid type.";
  {
    id: MultiHoleTyp,
    syntactic_form: [typ("Invalid")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let empty_hole: group = {id: EmptyHoleTyp, forms: [empty_hole_typ]};

let multi_hole: group = {id: MultiHoleTyp, forms: [multi_hole_typ]};
