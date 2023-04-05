/*open Example;
  open ExplainThisForm;

  let empty_hole_typ_group = "empty_hole_typ_group";
  let empty_hole_typ: form = {
    let explanation = {
      message: "Empty hole type. This marks a type that needs to be filled in.",
      feedback: Unselected,
    };
    {
      id: "empty_hole_typ",
      syntactic_form: [typ("EmptyHole")],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };

  // TODO Did get a case where in type position had space between two variables where got into weird state
  let multi_hole_typ_group = "multi_hole_typ_group";
  let multi_hole_typ: form = {
    let explanation = {
      message: "Multi hole type. This is an invalid type.",
      feedback: Unselected,
    };
    {
      id: "multi_hole_typ",
      syntactic_form: [typ("Invalid")],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };
  */
