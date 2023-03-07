/*open Haz3lcore;
  open ExampleUtil;
  open ExplainThisForm;

  let int_unary_minus_group = "int_unary_minus_group";
  let int_plus_group = "int_plus_group";
  let int_minus_group = "int_minus_group";
  let int_times_group = "int_times_group";
  let int_power_group = "int_power_group";
  let int_divide_group = "int_divide_group";
  let int_lt_group = "int_lt_group";
  let int_lte_group = "int_lte_group";
  let int_gt_group = "int_gt_group";
  let int_gte_group = "int_gte_group";
  let int_eq_group = "int_eq_group";
  let float_plus_group = "float_plus_group";
  let float_minus_group = "float_minus_group";
  let float_times_group = "float_times_group";
  let float_power_group = "float_power_group";
  let float_divide_group = "float_divide_group";
  let float_lt_group = "float_lt_group";
  let float_lte_group = "float_lte_group";
  let float_gt_group = "float_gt_group";
  let float_gte_group = "float_gte_group";
  let float_eq_group = "float_eq_group";
  let bool_and_group = "bool_and_group";
  let bool_or_group = "bool_or_group";
  let str_eq_group = "str_eq_group";
  let int_unary_minus_ex = {
    sub_id: "int_unary_minus_ex",
    term: mk_example("-1"),
    message: "The 1 is negated.",
    feedback: Unselected,
  };
  let int_plus_ex = {
    sub_id: "int_plus_ex",
    term: mk_example("1 + 2"),
    message: "1 added to 2 evalutes to 3.",
    feedback: Unselected,
  };
  let int_minus_ex = {
    sub_id: "int_minus_ex",
    term: mk_example("2 - 1"),
    message: "2 minus 1 evalutes to 1.",
    feedback: Unselected,
  };
  let int_times_ex = {
    sub_id: "int_times_ex",
    term: mk_example("1 * 2"),
    message: "1 multiplied be 2 evalutes to 2.",
    feedback: Unselected,
  };
  let int_power_ex = {
    sub_id: "int_power_ex",
    term: mk_example("2 ** 4"),
    message: "2 raised to 4 evaluates to 16",
    feedback: Unselected,
  };
  let int_divide_ex = {
    sub_id: "int_divide_ex",
    term: mk_example("6 / 3"),
    message: "6 divided by 3 evalutes to 2.",
    feedback: Unselected,
  };
  let int_lt1_ex = {
    sub_id: "int_lt1_ex",
    term: mk_example("1 < 2"),
    message: "1 is less than 2, so the expression evalutes to true.",
    feedback: Unselected,
  };
  let int_lt2_ex = {
    sub_id: "int_lt2_ex",
    term: mk_example("4 < 3"),
    message: "4 is less not less than 3, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let int_lte1_ex = {
    sub_id: "int_lte1_ex",
    term: mk_example("1 <= 2"),
    message: "1 is less than 2, so the expression evalutes to true.",
    feedback: Unselected,
  };
  let int_lte2_ex = {
    sub_id: "int_lte2_ex",
    term: mk_example("4 <= 3"),
    message: "4 is less not less than or equal to 3, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let int_lte3_ex = {
    sub_id: "int_lte3_ex",
    term: mk_example("5 <= 5"),
    message: "5 is equal to 5, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let int_gt1_ex = {
    sub_id: "int_gt1_ex",
    term: mk_example("1 > 2"),
    message: "1 is not greater than 2, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let int_gt2_ex = {
    sub_id: "int_gt2_ex",
    term: mk_example("4 > 3"),
    message: "4 is greater than 3, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let int_gte1_ex = {
    sub_id: "int_gte1_ex",
    term: mk_example("1 >= 2"),
    message: "1 is not greater than or equal to 2, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let int_gte2_ex = {
    sub_id: "int_gte2_ex",
    term: mk_example("4 >= 3"),
    message: "4 is greater than 3, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let int_gte3_ex = {
    sub_id: "int_gte3_ex",
    term: mk_example("5 >= 5"),
    message: "5 is equal to 5, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let int_eq1_ex = {
    sub_id: "int_eq1_ex",
    term: mk_example("1 == 2"),
    message: "1 does not equal 2, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let int_eq2_ex = {
    sub_id: "int_eq2_ex",
    term: mk_example("3 == 3"),
    message: "3 is equal to 3, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let float_plus_ex = {
    sub_id: "float_plus_ex",
    term: mk_example("1. +. 2.1"),
    message: "1. added to 2.1 evalutes to 3.1",
    feedback: Unselected,
  };
  let float_minus_ex = {
    sub_id: "float_minus_ex",
    term: mk_example("2. -. 1.1"),
    message: "2. minus 1.1 evalutes to 0.9",
    feedback: Unselected,
  };
  let float_times_ex = {
    sub_id: "float_times_ex",
    term: mk_example("1. *. 2.2"),
    message: "1 multiplied be 2.2 evalutes to 2.2.",
    feedback: Unselected,
  };
  let float_power_ex = {
    sub_id: "float_power_ex",
    term: mk_example("2. **. 4."),
    message: "2. raised to 4. evaluates to 16.",
    feedback: Unselected,
  };
  let float_divide_ex = {
    sub_id: "float_divide_ex",
    term: mk_example("4.2 /. 2.1"),
    message: "4.2 divided by 2.1 evalutes to 2.",
    feedback: Unselected,
  };
  let float_lt1_ex = {
    sub_id: "float_lt1_ex",
    term: mk_example("1. <. 2.1"),
    message: "1. is less than 2.1, so the expression evalutes to true.",
    feedback: Unselected,
  };
  let float_lt2_ex = {
    sub_id: "float_lt2_ex",
    term: mk_example("4. <. 3.1"),
    message: "4. is less not less than 3.1, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let float_lte1_ex = {
    sub_id: "float_lte1_ex",
    term: mk_example("1. <=. 2.1"),
    message: "1. is less than 2.1, so the expression evalutes to true.",
    feedback: Unselected,
  };
  let float_lte2_ex = {
    sub_id: "float_lte2_ex",
    term: mk_example("4. <=. 3.1"),
    message: "4. is less not less than or equal to 3.1, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let float_lte3_ex = {
    sub_id: "float_lte3_ex",
    term: mk_example("5.5 <=. 5.5"),
    message: "5.5 is equal to 5.5, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let float_gt1_ex = {
    sub_id: "float_gt1_ex",
    term: mk_example("1.1 >. 2.1"),
    message: "1.1 is not greater than 2.1, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let float_gt2_ex = {
    sub_id: "float_gt2_ex",
    term: mk_example("4. >. 3.1"),
    message: "4. is greater than 3.1, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let float_gte1_ex = {
    sub_id: "float_gte1_ex",
    term: mk_example("1.1 >=. 2.1"),
    message: "1.1 is not greater than or equal to 2.1, so the expression evaluates to false.",
    feedback: Unselected,
  };
  let float_gte2_ex = {
    sub_id: "float_gte2_ex",
    term: mk_example("4. >=. 3.1"),
    message: "4. is greater than 3.1, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let float_gte3_ex = {
    sub_id: "float_gte3_ex",
    term: mk_example("5.5 >=. 5.5"),
    message: "5.5 is equal to 5.5, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let float_eq1_ex = {
    sub_id: "float_eq1_ex",
    term: mk_example("1. ==. 2."),
    message: "1. does not equal 2., so the expression evaluates to false.",
    feedback: Unselected,
  };
  let float_eq2_ex = {
    sub_id: "float_eq2_ex",
    term: mk_example("3.1 ==. 3.1"),
    message: "3.1 is equal to 3.1, so the expression evaluates to true.",
    feedback: Unselected,
  };
  let bool_and1_ex = {
    sub_id: "bool_and1_ex",
    term: mk_example("true && false"),
    message: "The left operand is true, so evaluate the right operand. Since the right operand is false, the whole expression evaluates to false.",
    feedback: Unselected,
  };
  let bool_and2_ex = {
    sub_id: "bool_and2_ex",
    term: mk_example("1 < 2 && 3 < 4"),
    message: "The left operand evaluates to true, so evaluate the right operand. Since the right operand also evalutes to true, the whole expression evaluates to true.",
    feedback: Unselected,
  };
  let bool_or1_ex = {
    sub_id: "bool_or1_ex",
    term: mk_example("false || 2 < 1"),
    message: "The left operand evaluates to false, so evaluate the right operand. Since the right operand also evaluates to false, the whole expression evaluates to false.",
    feedback: Unselected,
  };
  let bool_or2_ex = {
    sub_id: "bool_or2_ex",
    term: mk_example("3 < 4 || false"),
    message: "The left operand evalutes to true, so the right operand is not evaluated. The whole expression evaluates to true.",
    feedback: Unselected,
  };
  let str_eq1_ex = {
    sub_id: "str_eq1_ex",
    term: mk_example("\"abc\" $== \"xyz\""),
    message: "\"abc\" does not equal \"xyz\", so the expression evaluates to false.",
    feedback: Unselected,
  };
  let str_eq2_ex = {
    sub_id: "str_eq2_ex",
    term: mk_example("\"abc\" $== \"abc\""),
    message: "\"abc\" is equal to \"abc\", so the expression evaluates to true.",
    feedback: Unselected,
  };
  let _exp = exp("e");
  let int_unary_minus_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_exp), exp_id),
  ];
  let int_unary_minus_exp: form = {
    let explanation = {
      message: "Unary minus. Performs integer negation of the [*operand*](%i).",
      feedback: Unselected,
    };
    {
      id: "int_unary_minus_exp",
      syntactic_form: [unary_minus(), _exp],
      expandable_id: None,
      explanation,
      examples: [int_unary_minus_ex],
    };
  };
  let _binop_exp_coloring_ids =
      (sf_left_id: Id.t, sf_right_id: Id.t, ~left_id: Id.t, ~right_id: Id.t)
      : list((Id.t, Id.t)) => {
    [(sf_left_id, left_id), (sf_right_id, right_id)];
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_plus_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_plus_exp: form = {
    let explanation = {
      message: "Integer addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "int_plus_exp",
      syntactic_form: [_exp1, space(), plus(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_plus_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_minus_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_minus_exp: form = {
    let explanation = {
      message: "Integer subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "int_minus_exp",
      syntactic_form: [_exp1, space(), minus(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_minus_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_times_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_times_exp: form = {
    let explanation = {
      message: "Integer multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "int_times_exp",
      syntactic_form: [_exp1, space(), times(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_times_ex],
    };
  };
  let int_power_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_power_exp: form = {
    let explanation = {
      message: "Integer exponentiation. Gives the result of raising [*left*](%i) ro the [*right*](%i).",
      feedback: Unselected,
    };
    {
      id: "int_power_exp",
      syntactic_form: [_exp1, space(), power(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_power_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_divide_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_divide_exp: form = {
    let explanation = {
      message: "Integer division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "int_divide_exp",
      syntactic_form: [_exp1, space(), divide(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_divide_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_lt_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_lt_exp: form = {
    let explanation = {
      message: "Integer less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "int_lt_exp",
      syntactic_form: [_exp1, space(), lt(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_lt1_ex, int_lt2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_lte_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_lte_exp: form = {
    let explanation = {
      message: "Integer less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "int_lte_exp",
      syntactic_form: [_exp1, space(), lte(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_lte1_ex, int_lte2_ex, int_lte3_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_gt_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_gt_exp: form = {
    let explanation = {
      message: "Integer greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "int_gt_exp",
      syntactic_form: [_exp1, space(), gt(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_gt1_ex, int_gt2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_gte_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_gte_exp: form = {
    let explanation = {
      message: "Integer greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "int_gte_exp",
      syntactic_form: [_exp1, space(), gte(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_gte1_ex, int_gte2_ex, int_gte3_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let int_eq_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let int_eq_exp: form = {
    let explanation = {
      message: "Integer equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "int_eq_exp",
      syntactic_form: [_exp1, space(), equals(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [int_eq1_ex, int_eq2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_plus_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_plus_exp: form = {
    let explanation = {
      message: "Floating-point addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "float_plus_exp",
      syntactic_form: [_exp1, space(), fplus(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_plus_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_minus_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_minus_exp: form = {
    let explanation = {
      message: "Floating-point subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "float_minus_exp",
      syntactic_form: [_exp1, space(), fminus(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_minus_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_times_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_times_exp: form = {
    let explanation = {
      message: "Floating-point multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "float_times_exp",
      syntactic_form: [_exp1, space(), ftimes(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_times_ex],
    };
  };
  let float_power_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_power_exp: form = {
    let explanation = {
      message: "Floating-point exponentiation.  Gives the result of raising [*left*](%i) to the [*right*](%i).",
      feedback: Unselected,
    };
    {
      id: "float_power_exp",
      syntactic_form: [_exp1, space(), fpower(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_power_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_divide_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_divide_exp: form = {
    let explanation = {
      message: "Floating-point division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.",
      feedback: Unselected,
    };
    {
      id: "float_divide_exp",
      syntactic_form: [_exp1, space(), fdivide(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_divide_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_lt_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_lt_exp: form = {
    let explanation = {
      message: "Floating-point less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "float_lt_exp",
      syntactic_form: [_exp1, space(), flt(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_lt1_ex, float_lt2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_lte_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_lte_exp: form = {
    let explanation = {
      message: "Floating-point less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "float_lte_exp",
      syntactic_form: [_exp1, space(), flte(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_lte1_ex, float_lte2_ex, float_lte3_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_gt_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_gt_exp: form = {
    let explanation = {
      message: "Floating-point greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "float_gt_exp",
      syntactic_form: [_exp1, space(), fgt(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_gt1_ex, float_gt2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_gte_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_gte_exp: form = {
    let explanation = {
      message: "Floating-point greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "float_gte_exp",
      syntactic_form: [_exp1, space(), fgte(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_gte1_ex, float_gte2_ex, float_gte3_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let float_eq_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let float_eq_exp: form = {
    let explanation = {
      message: "Floating-point equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "float_eq_exp",
      syntactic_form: [_exp1, space(), fequals(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [float_eq1_ex, float_eq2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let bool_and_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let bool_and_exp: form = {
    let explanation = {
      message: "Boolean and. If the [*left operand*](%i) evaluates to `true`, evaluate the [*right operand*](%i). If that also evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "bool_and_exp",
      syntactic_form: [_exp1, space(), logical_and(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [bool_and1_ex, bool_and2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let bool_or_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  // TODO Some of the examples are evaluating weirdly and can't type the || in the editor
  let bool_or_exp: form = {
    let explanation = {
      message: "Boolean or. If the [*left operand*](%i) evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluate the [*right operand*](%i). If that evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "bool_or_exp",
      syntactic_form: [_exp1, space(), logical_or(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [bool_or1_ex, bool_or2_ex],
    };
  };
  let _exp1 = exp("e1");
  let _exp2 = exp("e2");
  let str_eq_exp_coloring_ids =
      (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
    _binop_exp_coloring_ids(
      Piece.id(_exp1),
      Piece.id(_exp2),
      ~left_id,
      ~right_id,
    );
  let str_eq_exp: form = {
    let explanation = {
      message: "String equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
      feedback: Unselected,
    };
    {
      id: "str_eq_exp",
      syntactic_form: [_exp1, space(), sequals(), space(), _exp2],
      expandable_id: None,
      explanation,
      examples: [str_eq1_ex, str_eq2_ex],
    };
  };*/
