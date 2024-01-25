open ExplainThisForm;
open Haz3lcore;

let empty_hole_template = (_sort, str, id): form => {
  let explanation =
    Printf.sprintf(
      "This should be filled with %s to complete the program.",
      str,
    );
  {
    id,
    syntactic_form: [Grout({id: Id.mk(), shape: Convex})],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_template = (sort, id): form => {
  let explanation = "This is an invalid term.";
  {
    id,
    syntactic_form: [sort("INVALID")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
