open Example;
open ExplainThisForm;

let empty_hole_typ: form = {
  let explanation = "Empty hole type. This marks a type that needs to be filled in.";
  {
    id: EmptyHoleTyp,
    syntactic_form: [typ("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

// TODO Did get a case where in type position had space between two variables where got into weird state
let multi_hole_typ: form = {
  let explanation = "Multi hole type. This is an invalid type.";
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
