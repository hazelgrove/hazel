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
  term: mk_example("false || 2 < 1"),
  message: "The left operand evaluates to false, so evaluate the right operand. Since the right operand also evaluates to false, the whole expression evaluates to false.",
};
let bool_or2_ex = {
  sub_id: OrTrue,
  term: mk_example("3 < 4 || false"),
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
let _exp = exp("e");
let int_unary_minus_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp), exp_id),
];
let int_unary_minus_exp: form = {
  let explanation = "Unary minus. Performs integer negation of the [*operand*](%i).";
  {
    id: UnOpExp(IntMinus),
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
  let explanation = "Integer addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(IntPlus),
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
  let explanation = "Integer subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(IntMinus),
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
  let explanation = "Integer multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(IntTimes),
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
  let explanation = "Integer exponentiation. Gives the result of raising [*left*](%i) ro the [*right*](%i).";
  {
    id: BinOpExp(IntPower),
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
  let explanation = "Integer division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(IntDivide),
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
  let explanation = "Integer less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(IntLessThan),
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
  let explanation = "Integer less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(IntLessThanEqual),
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
  let explanation = "Integer greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(IntGreaterThan),
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
  let explanation = "Integer greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(IntGreaterThanEqual),
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
  let explanation = "Integer equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(IntEqual),
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
  let explanation = "Floating-point addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(FloatPlus),
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
  let explanation = "Floating-point subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(FloatMinus),
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
  let explanation = "Floating-point multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(FloatTimes),
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
  let explanation = "Floating-point exponentiation.  Gives the result of raising [*left*](%i) to the [*right*](%i).";
  {
    id: BinOpExp(FloatPower),
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
  let explanation = "Floating-point division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.";
  {
    id: BinOpExp(FloatDivide),
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
  let explanation = "Floating-point less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(FloatLessThan),
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
  let explanation = "Floating-point less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(FloatLessThanEqual),
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
  let explanation = "Floating-point greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(FloatGreaterThan),
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
  let explanation = "Floating-point greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.";
  {
    id: BinOpExp(FloatGreaterThanEqual),
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
  let explanation = "Floating-point equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(FloatEqual),
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
  let explanation = "Boolean and. If the [*left operand*](%i) evaluates to `true`, evaluate the [*right operand*](%i). If that also evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(And),
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
  let explanation = "Boolean or. If the [*left operand*](%i) evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluate the [*right operand*](%i). If that evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(Or),
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
  let explanation = "String equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.";
  {
    id: BinOpExp(StringEqual),
    syntactic_form: [_exp1, space(), sequals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [str_eq1_ex, str_eq2_ex],
  };
};

let int_un_minus: group = {
  id: UnOpExp(IntMinus),
  forms: [int_unary_minus_exp],
};

let int_plus: group = {id: BinOpExp(IntPlus), forms: [int_plus_exp]};

let int_minus: group = {id: BinOpExp(IntMinus), forms: [int_minus_exp]};

let int_times: group = {id: BinOpExp(IntTimes), forms: [int_times_exp]};

let int_power: group = {id: BinOpExp(IntPower), forms: [int_power_exp]};

let int_divide: group = {id: BinOpExp(IntDivide), forms: [int_divide_exp]};

let int_less_than: group = {
  id: BinOpExp(IntLessThan),
  forms: [int_lte_exp],
};

let int_less_than_equal: group = {
  id: BinOpExp(IntLessThanEqual),
  forms: [int_lte_exp],
};

let int_greater_than: group = {
  id: BinOpExp(IntGreaterThan),
  forms: [int_gt_exp],
};

let int_greater_than_equal: group = {
  id: BinOpExp(IntGreaterThanEqual),
  forms: [int_gte_exp],
};

let int_equal: group = {id: BinOpExp(FloatEqual), forms: [int_eq_exp]};

let float_plus: group = {id: BinOpExp(FloatPlus), forms: [float_plus_exp]};

let float_minus: group = {
  id: BinOpExp(FloatMinus),
  forms: [float_minus_exp],
};

let float_times: group = {
  id: BinOpExp(FloatTimes),
  forms: [float_times_exp],
};

let float_power: group = {
  id: BinOpExp(FloatPower),
  forms: [float_power_exp],
};

let float_divide: group = {
  id: BinOpExp(FloatDivide),
  forms: [float_divide_exp],
};

let float_less_than: group = {
  id: BinOpExp(FloatLessThan),
  forms: [float_lte_exp],
};

let float_less_than_equal: group = {
  id: BinOpExp(FloatLessThanEqual),
  forms: [float_lte_exp],
};

let float_greater_than: group = {
  id: BinOpExp(FloatGreaterThan),
  forms: [float_gt_exp],
};

let float_greater_than_equal: group = {
  id: BinOpExp(FloatGreaterThanEqual),
  forms: [float_gte_exp],
};

let float_equal: group = {id: BinOpExp(FloatEqual), forms: [float_eq_exp]};

let bool_and: group = {id: BinOpExp(And), forms: [bool_and_exp]};

let bool_or: group = {id: BinOpExp(Or), forms: [bool_or_exp]};

let string_equal: group = {id: BinOpExp(StringEqual), forms: [str_eq_exp]};
