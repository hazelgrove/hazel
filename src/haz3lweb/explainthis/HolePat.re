/*open ExampleUtil;
  open ExplainThisForm;

  let empty_hole_pat_group = "empty_hole_pat_group";
  let empty_hole_pat: form = {
    let explanation = {
      message: "Empty hole pattern. Expressions are not matched against the *empty hole pattern* until it is filled.",
      feedback: Unselected,
    };
    {
      id: "empty_hole_pat",
      syntactic_form: [pat("EmptyHole")],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };

  let multi_hole_pat_group = "multi_hole_pat_group";
  let multi_hole_pat: form = {
    let explanation = {
      message: "Unrecognized pattern. Expressions are not matched against the invalid pattern until it is corrected.",
      feedback: Unselected,
    };
    {
      id: "multi_hole_pat",
      syntactic_form: [pat("Invalid")],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };*/
