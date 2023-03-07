/*open Haz3lcore;
  open ExplainThisForm;
  open ExampleUtil;

  let tuple_exp_group = "tuple_exp_group";
  let tuple_exp_2_group = "tuple_exp_2_group";
  let tuple_exp_3_group = "tuple_exp_3_group";
  let tuple_example_1 = {
    sub_id: "tuple_example_1",
    term: mk_example("(true, 1)"),
    message: "A tuple with first elment true and second element 1.",
    feedback: Unselected,
  };
  let tuple_example_2 = {
    sub_id: "tuple_example_2",
    term: mk_example("(1, 2, 3)"),
    message: "A tuple with first element 1, second element 2, and third element 3.",
    feedback: Unselected,
  };
  let tuple_exp: form = {
    let explanation = {
      message: "Tuple literal. The tuple has %i elements.",
      feedback: Unselected,
    };
    let comma = comma_exp();
    {
      id: "tuple_exp",
      syntactic_form: [exp("e1"), comma, space(), exp("...")],
      expandable_id: Some(Piece.id(comma)),
      explanation,
      examples: [tuple_example_1, tuple_example_2],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let tuple_exp_size2_coloring_ids =
      (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => {
    [(Piece.id(_exp1), exp1_id), (Piece.id(_exp2), exp2_id)];
  };
  let tuple_exp_size2: form = {
    let explanation = {
      message: "Tuple literal. The 2-tuple has a [first](%i) and [second](%i) element.",
      feedback: Unselected,
    };
    let comma = comma_exp();
    {
      id: "tuple_exp_size2",
      syntactic_form: [_exp1, comma, space(), _exp2],
      expandable_id: Some(Piece.id(comma)),
      explanation,
      examples: [tuple_example_1],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let _exp3 = exp("e3");
  let tuple_exp_size3_coloring_ids =
      (~exp1_id: Id.t, ~exp2_id: Id.t, ~exp3_id: Id.t): list((Id.t, Id.t)) => {
    [
      (Piece.id(_exp1), exp1_id),
      (Piece.id(_exp2), exp2_id),
      (Piece.id(_exp3), exp3_id),
    ];
  };
  let tuple_exp_size3: form = {
    let explanation = {
      message: "Tuple literal. The 3-tuple has a [first](%i), [second](%i), and [third](%i) element.",
      feedback: Unselected,
    };
    let comma = comma_exp();
    {
      id: "tuple_exp_size3",
      syntactic_form: [
        _exp1,
        comma_exp(),
        space(),
        _exp2,
        comma,
        space(),
        _exp3,
      ],
      expandable_id: Some(Piece.id(comma)),
      explanation,
      examples: [tuple_example_2],
    };
  };*/
