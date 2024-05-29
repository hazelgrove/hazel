open Haz3lcore;
open Example;
open ExplainThisForm;

let int_unary_minus_ex = {
  sub_id: IntUnaryMinus,
  term: mk_example("-1"),
  message: "The 1 is negated.",
};
let int_plus_ex = {
  sub_id: Int(Plus),
  term: mk_example("1 + 2"),
  message: "1 added to 2 evalutes to 3.",
};
let int_minus_ex = {
  sub_id: Int(Minus),
  term: mk_example("2 - 1"),
  message: "2 minus 1 evalutes to 1.",
};
let int_times_ex = {
  sub_id: Int(Times),
  term: mk_example("1 * 2"),
  message: "1 multiplied be 2 evalutes to 2.",
};
let int_power_ex = {
  sub_id: Int(Power),
  term: mk_example("2 ** 4"),
  message: "2 raised to 4 evaluates to 16",
};
let int_divide_ex = {
  sub_id: Int(Divide),
  term: mk_example("6 / 3"),
  message: "6 divided by 3 evalutes to 2.",
};
let int_lt1_ex = {
  sub_id: Int(LessThanTrue),
  term: mk_example("1 < 2"),
  message: "1 is less than 2, so the expression evalutes to true.",
};
let int_lt2_ex = {
  sub_id: Int(LessThanFalse),
  term: mk_example("4 < 3"),
  message: "4 is less not less than 3, so the expression evaluates to false.",
};
let int_lte1_ex = {
  sub_id: Int(LessThanEqualLess),
  term: mk_example("1 <= 2"),
  message: "1 is less than 2, so the expression evalutes to true.",
};
let int_lte2_ex = {
  sub_id: Int(LessThanEqualFalse),
  term: mk_example("4 <= 3"),
  message: "4 is less not less than or equal to 3, so the expression evaluates to false.",
};
let int_lte3_ex = {
  sub_id: Int(LessThanEqualEqual),
  term: mk_example("5 <= 5"),
  message: "5 is equal to 5, so the expression evaluates to true.",
};
let int_gt1_ex = {
  sub_id: Int(GreaterThanFalse),
  term: mk_example("1 > 2"),
  message: "1 is not greater than 2, so the expression evaluates to false.",
};
let int_gt2_ex = {
  sub_id: Int(GreaterThanTrue),
  term: mk_example("4 > 3"),
  message: "4 is greater than 3, so the expression evaluates to true.",
};
let int_gte1_ex = {
  sub_id: Int(GreaterThanEqualFalse),
  term: mk_example("1 >= 2"),
  message: "1 is not greater than or equal to 2, so the expression evaluates to false.",
};
let int_gte2_ex = {
  sub_id: Int(GreaterThanEqualGreater),
  term: mk_example("4 >= 3"),
  message: "4 is greater than 3, so the expression evaluates to true.",
};
let int_gte3_ex = {
  sub_id: Int(GreaterThanEqualEqual),
  term: mk_example("5 >= 5"),
  message: "5 is equal to 5, so the expression evaluates to true.",
};
let int_eq1_ex = {
  sub_id: Int(EqualFalse),
  term: mk_example("1 == 2"),
  message: "1 does not equal 2, so the expression evaluates to false.",
};
let int_eq2_ex = {
  sub_id: Int(EqualTrue),
  term: mk_example("3 == 3"),
  message: "3 is equal to 3, so the expression evaluates to true.",
};
let int_poly_eq1_ex = {
  sub_id: Int(PolyEqualFalse),
  term: mk_example("1.0 == 2.0"),
  message: "1.0 does not equal 2.0, so the expression evaluates to false.",
};
let int_poly_eq2_ex = {
  sub_id: Int(PolyEqualTrue),
  term: mk_example("true == true"),
  message: "true is equal to true, so the expression evaluates to true.",
};
let float_plus_ex = {
  sub_id: Float(Plus),
  term: mk_example("1. +. 2.1"),
  message: "1. added to 2.1 evalutes to 3.1",
};
let float_minus_ex = {
  sub_id: Float(Minus),
  term: mk_example("2. -. 1.1"),
  message: "2. minus 1.1 evalutes to 0.9",
};
let float_times_ex = {
  sub_id: Float(Times),
  term: mk_example("1. *. 2.2"),
  message: "1 multiplied be 2.2 evalutes to 2.2.",
};
let float_power_ex = {
  sub_id: Float(Power),
  term: mk_example("2. **. 4."),
  message: "2. raised to 4. evaluates to 16.",
};
let float_divide_ex = {
  sub_id: Float(Divide),
  term: mk_example("4.2 /. 2.1"),
  message: "4.2 divided by 2.1 evalutes to 2.",
};
let float_lt1_ex = {
  sub_id: Float(LessThanTrue),
  term: mk_example("1. <. 2.1"),
  message: "1. is less than 2.1, so the expression evalutes to true.",
};
let float_lt2_ex = {
  sub_id: Float(LessThanFalse),
  term: mk_example("4. <. 3.1"),
  message: "4. is less not less than 3.1, so the expression evaluates to false.",
};
let float_lte1_ex = {
  sub_id: Float(LessThanEqualLess),
  term: mk_example("1. <=. 2.1"),
  message: "1. is less than 2.1, so the expression evalutes to true.",
};
let float_lte2_ex = {
  sub_id: Float(LessThanEqualFalse),
  term: mk_example("4. <=. 3.1"),
  message: "4. is less not less than or equal to 3.1, so the expression evaluates to false.",
};
let float_lte3_ex = {
  sub_id: Float(LessThanEqualEqual),
  term: mk_example("5.5 <=. 5.5"),
  message: "5.5 is equal to 5.5, so the expression evaluates to true.",
};
let float_gt1_ex = {
  sub_id: Float(GreaterThanFalse),
  term: mk_example("1.1 >. 2.1"),
  message: "1.1 is not greater than 2.1, so the expression evaluates to false.",
};
let float_gt2_ex = {
  sub_id: Float(GreaterThanTrue),
  term: mk_example("4. >. 3.1"),
  message: "4. is greater than 3.1, so the expression evaluates to true.",
};
let float_gte1_ex = {
  sub_id: Float(GreaterThanEqualFalse),
  term: mk_example("1.1 >=. 2.1"),
  message: "1.1 is not greater than or equal to 2.1, so the expression evaluates to false.",
};
let float_gte2_ex = {
  sub_id: Float(GreaterThanEqualGreater),
  term: mk_example("4. >=. 3.1"),
  message: "4. is greater than 3.1, so the expression evaluates to true.",
};
let float_gte3_ex = {
  sub_id: Float(GreaterThanEqualEqual),
  term: mk_example("5.5 >=. 5.5"),
  message: "5.5 is equal to 5.5, so the expression evaluates to true.",
};
let float_eq1_ex = {
  sub_id: Float(EqualFalse),
  term: mk_example("1. ==. 2."),
  message: "1. does not equal 2., so the expression evaluates to false.",
};
let float_eq2_ex = {
  sub_id: Float(EqualTrue),
  term: mk_example("3.1 ==. 3.1"),
  message: "3.1 is equal to 3.1, so the expression evaluates to true.",
};
let bool_and1_ex = {
  sub_id: AndFalse,
  term: mk_example("true && false"),
  message: "The left operand is true, so evaluate the right operand. Since the right operand is false, the whole expression evaluates to false.",
};
let bool_and2_ex = {
  sub_id: AndTrue,
  term: mk_example("1 < 2 && 3 < 4"),
  message: "The left operand evaluates to true, so evaluate the right operand. Since the right operand also evalutes to true, the whole expression evaluates to true.",
};
let bool_or1_ex = {
  sub_id: OrFalse,
  term: mk_example("false \\/ 2 < 1"),
  message: "The left operand evaluates to false, so evaluate the right operand. Since the right operand also evaluates to false, the whole expression evaluates to false.",
};
let bool_or2_ex = {
  sub_id: OrTrue,
  term: mk_example("3 < 4 \\/ false"),
  message: "The left operand evalutes to true, so the right operand is not evaluated. The whole expression evaluates to true.",
};
let str_eq1_ex = {
  sub_id: StringEqualFalse,
  term: mk_example("\"abc\" $== \"xyz\""),
  message: "\"abc\" does not equal \"xyz\", so the expression evaluates to false.",
};
let str_eq2_ex = {
  sub_id: StringEqualTrue,
  term: mk_example("\"abc\" $== \"abc\""),
  message: "\"abc\" is equal to \"abc\", so the expression evaluates to true.",
};
let _unop_exp_coloring_ids =
    (sf_exp_id: Id.t, ~exp_id: Id.t): list((Id.t, Id.t)) => [
  (sf_exp_id, exp_id),
];
let _exp = exp("e");
let bool_unary_not_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) =>
  _unop_exp_coloring_ids(Piece.id(_exp), ~exp_id);
let bool_unary_not_exp: form = {
  let explanation = "Performs boolean negation of the [*operand*](%s).";
  {
    id: UnOpExp(Bool(Not)),
    syntactic_form: [unary_not(), _exp],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let _exp = exp("e");
let int_unary_minus_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) =>
  _unop_exp_coloring_ids(Piece.id(_exp), ~exp_id);
let int_unary_minus_exp: form = {
  let explanation = "Performs integer negation of the [*operand*](%s).";
  {
    id: UnOpExp(Int(Minus)),
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
  let explanation = "Gives the sum of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Int(Plus)),
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
  let explanation = "Gives the difference of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Int(Minus)),
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
  let explanation = "Gives the product of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Int(Times)),
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
  let explanation = "Gives the result of raising [*left*](%s) ro the [*right*](%s).";
  {
    id: BinOpExp(Int(Power)),
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
  let explanation = "Gives the quotient of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Int(Divide)),
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
  let explanation = "If the [*left operand*](%s) is less than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Int(LessThan)),
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
  let explanation = "If the [*left operand*](%s) is less than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Int(LessThanOrEqual)),
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
  let explanation = "If the [*left operand*](%s) is greater than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Int(GreaterThan)),
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
  let explanation = "If the [*left operand*](%s) is greater than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Int(GreaterThanOrEqual)),
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
  let explanation = "Performs a polymorphic comparison. If the [*left operand*](%s) is equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(Int(Equals)),
    syntactic_form: [_exp1, space(), equals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_eq1_ex, int_eq2_ex, int_poly_eq1_ex, int_poly_eq2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_neq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_neq_exp: form = {
  let explanation = "Performs a polymorphic comparison. If the [*left operand*](%s) is not equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(Int(NotEquals)),
    syntactic_form: [_exp1, space(), not_equals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [],
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
  let explanation = "Gives the sum of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Float(Plus)),
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
  let explanation = "Gives the difference of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Float(Minus)),
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
  let explanation = "Gives the product of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Float(Times)),
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
  let explanation = "Gives the result of raising [*left*](%s) to the [*right*](%s).";
  {
    id: BinOpExp(Float(Power)),
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
  let explanation = "Gives the quotient of the [*left*](%s) and [*right*](%s) operands.";
  {
    id: BinOpExp(Float(Divide)),
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
  let explanation = "If the [*left operand*](%s) is less than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Float(LessThan)),
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
  let explanation = "If the [*left operand*](%s) is less than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Float(LessThanOrEqual)),
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
  let explanation = "If the [*left operand*](%s) is greater than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Float(GreaterThan)),
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
  let explanation = "If the [*left operand*](%s) is greater than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(Float(GreaterThanOrEqual)),
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
  let explanation = "If the [*left operand*](%s) is equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(Float(Equals)),
    syntactic_form: [_exp1, space(), fequals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_eq1_ex, float_eq2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_neq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_neq_exp: form = {
  let explanation = "If the [*left operand*](%s) is not equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(Float(NotEquals)),
    syntactic_form: [_exp1, space(), fnot_equals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [],
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
  let explanation = "If the [*left operand*](%s) evaluates to `true`, evaluate the [*right operand*](%s). If that also evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(Bool(And)),
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
  let explanation = "If the [*left operand*](%s) evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluate the [*right operand*](%s). If that evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(Bool(Or)),
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
  let explanation = "If the [*left operand*](%s) is equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(String(Equals)),
    syntactic_form: [_exp1, space(), sequals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [str_eq1_ex, str_eq2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let str_concat_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let str_concat_exp: form = {
  let explanation = "Returns the concatenation of the [*left operand*](%s) and the [*right operand*](%s),";
  {
    id: BinOpExp(String(Concat)),
    syntactic_form: [_exp1, space(), sconcat(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let bool_un_not: group = {
  id: UnOpExp(Bool(Not)),
  forms: [bool_unary_not_exp],
};

let int_un_minus: group = {
  id: UnOpExp(Int(Minus)),
  forms: [int_unary_minus_exp],
};

let int_plus: group = {id: BinOpExp(Int(Plus)), forms: [int_plus_exp]};

let int_minus: group = {id: BinOpExp(Int(Minus)), forms: [int_minus_exp]};

let int_times: group = {id: BinOpExp(Int(Times)), forms: [int_times_exp]};

let int_power: group = {id: BinOpExp(Int(Power)), forms: [int_power_exp]};

let int_divide: group = {
  id: BinOpExp(Int(Divide)),
  forms: [int_divide_exp],
};

let int_less_than: group = {
  id: BinOpExp(Int(LessThan)),
  forms: [int_lt_exp],
};

let int_less_than_equal: group = {
  id: BinOpExp(Int(LessThanOrEqual)),
  forms: [int_lte_exp],
};

let int_greater_than: group = {
  id: BinOpExp(Int(GreaterThan)),
  forms: [int_gt_exp],
};

let int_greater_than_equal: group = {
  id: BinOpExp(Int(GreaterThanOrEqual)),
  forms: [int_gte_exp],
};

let int_equal: group = {id: BinOpExp(Int(Equals)), forms: [int_eq_exp]};

let int_not_equal: group = {
  id: BinOpExp(Int(NotEquals)),
  forms: [int_neq_exp],
};

let float_plus: group = {
  id: BinOpExp(Float(Plus)),
  forms: [float_plus_exp],
};

let float_minus: group = {
  id: BinOpExp(Float(Minus)),
  forms: [float_minus_exp],
};

let float_times: group = {
  id: BinOpExp(Float(Times)),
  forms: [float_times_exp],
};

let float_power: group = {
  id: BinOpExp(Float(Power)),
  forms: [float_power_exp],
};

let float_divide: group = {
  id: BinOpExp(Float(Divide)),
  forms: [float_divide_exp],
};

let float_less_than: group = {
  id: BinOpExp(Float(LessThan)),
  forms: [float_lt_exp],
};

let float_less_than_equal: group = {
  id: BinOpExp(Float(LessThanOrEqual)),
  forms: [float_lte_exp],
};

let float_greater_than: group = {
  id: BinOpExp(Float(GreaterThan)),
  forms: [float_gt_exp],
};

let float_greater_than_equal: group = {
  id: BinOpExp(Float(GreaterThanOrEqual)),
  forms: [float_gte_exp],
};

let float_equal: group = {
  id: BinOpExp(Float(Equals)),
  forms: [float_eq_exp],
};

let float_not_equal: group = {
  id: BinOpExp(Float(NotEquals)),
  forms: [float_neq_exp],
};

let bool_and: group = {id: BinOpExp(Bool(And)), forms: [bool_and_exp]};

let bool_or: group = {id: BinOpExp(Bool(Or)), forms: [bool_or_exp]};

let string_equal: group = {
  id: BinOpExp(String(Equals)),
  forms: [str_eq_exp],
};

let string_concat: group = {
  id: BinOpExp(String(Concat)),
  forms: [str_concat_exp],
};
