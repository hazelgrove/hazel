open ExplainThisForm;

let empty_hole_template = (sort, str, id): form => {
  let explanation =
    Printf.sprintf(
      "Empty hole. This should be filled with %s to complete the program.",
      str,
    );
  {
    id,
    syntactic_form: [sort("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_template = (sort, id): form => {
  let explanation = "Not recognized. This is an invalid term.";
  {
    id,
    syntactic_form: [sort("INVALID")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
