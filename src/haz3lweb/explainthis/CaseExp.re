/*open Haz3lcore;
  open ExampleUtil;
  open ExplainThisForm;

  let case_exp_group = "case_exp_group";
  let case_rules_group = "case_rules_group";
  let case_example_wild_simple = {
    sub_id: "case_example_wild_simple",
    term: mk_example("case 1 \n| 2 => 3 \n| _ => 4 \nend"),
    message: "The scrutinee of the case expression is 1. Since the scrutinee does not match the the first pattern 2. Since the scrutinee does match the second pattern which is a wildcard, the second branch is taken. The whole expression evaluates to the second clause 4.",
    feedback: Unselected,
  };
  let case_example_wild_tuple = {
    sub_id: "case_example_wild_tuple",
    term: mk_example("case (1, 2) \n| (_, 2) => 3 \n| _ => 4 \nend"),
    message: "The scrutinee of the case expression is (1, 2). Since the scrutinee matches the first pattern (_, 2), the first branch is taken. This pattern is matched because the first element 1 matches the first element pattern, which is a wildcard, and the second element 2 matches the second element pattern 2. The whole expression evaluates to the first clause 3.",
    feedback: Unselected,
  };
  let case_example_int = {
    sub_id: "case_example_int",
    term: mk_example("case 1 \n| 1 => 1.1 \n| 2 => 2.2 \n| _ => 3.3 \nend"),
    message: "The scrutinee of the case expression is 1. Since the scrutinee matches the first pattern 1, the first branch is taken. The whole expression evaluates to the first clause 1.1.",
    feedback: Unselected,
  };
  let case_example_bool = {
    sub_id: "case_example_bool",
    term: mk_example("case false \n| true => 1 | \nfalse => 2 \nend"),
    message: "The scrutinee of the case expression is false. The scrutinee does not match the first pattern true. Since, scrutinee does match the second pattern false, the second branch is taken. The whole expression evaluates to the second clause 2.",
    feedback: Unselected,
  };
  // TODO - I don't think changing specificity on the number of cases is really the most
  // beneficial specificity change - I think instead have generic at top level
  // and then have a slightly different setup for specific that is created more
  // dynamically calling setup methods here but more
  // work done in the ExplainThis code - maybe just up to 3 or 4 branches?
  let _exp_scrut = exp("e_scrut");
  let case_exp_coloring_ids = (~scrut_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_exp_scrut), scrut_id),
  ];
  let case_exp: form = {
    let explanation = {
      message: "Case expression. Consider each branch in order. For the first branch with a *pattern* that matches the [*scrutinee*](%i), evaluates to the corresponding *clause*.",
      feedback: Unselected,
    };
    let case =
      mk_case([
        [
          space(),
          _exp_scrut,
          linebreak(),
          mk_rule([[space(), pat("p1"), space()]]),
          space(),
          exp("e1"),
          linebreak(),
          mk_rule([[space(), pat("..."), space()]]),
          space(),
          exp("..."),
          linebreak(),
        ],
      ]);
    {
      id: "case_exp",
      syntactic_form: [case],
      expandable_id: Some(Piece.id(case)),
      explanation,
      examples: [case_example_int, case_example_bool],
    };
  };*/
/*let case_exp_rules: form = {
    let explanation = {
      message: "Case expression. Consider each branch in order. If the [*scrutinee*] matches:",
      feedback: Unselected,
    };
    let case =
      mk_case([
        [
          exp("EXP_scrut"),
          mk_rule([[pat("PAT1")]]),
          exp("EXP1"),
          mk_rule([[pat("...")]]),
          exp("..."),
        ],
      ]);
    {
      id: "case_exp_rules",
      syntactic_form: [case],
      expandable_id: Some(Piece.id(case)),
      explanation,
      examples: [case_example_int, case_example_bool],
    };
  };*/
