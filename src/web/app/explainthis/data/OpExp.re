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
  message: "1 added to 2 evaluates to 3.",
};
let int_minus_ex = {
  sub_id: Int(Minus),
  term: mk_example("2 - 1"),
  message: "2 minus 1 evaluates to 1.",
};
let int_times_ex = {
  sub_id: Int(Times),
  term: mk_example("1 * 2"),
  message: "1 multiplied by 2 evaluates to 2.",
};
let int_power_ex = {
  sub_id: Int(Power),
  term: mk_example("2 ** 4"),
  message: "2 raised to 4 evaluates to 16",
};
let int_divide_ex = {
  sub_id: Int(Divide),
  term: mk_example("6 / 3"),
  message: "6 divided by 3 evaluates to 2.",
};
let int_lt1_ex = {
  sub_id: Int(LessThanTrue),
  term: mk_example("1 < 2"),
  message: "1 is less than 2, so the expression evaluates to true.",
};
let int_lt2_ex = {
  sub_id: Int(LessThanFalse),
  term: mk_example("4 < 3"),
  message: "4 is less not less than 3, so the expression evaluates to false.",
};
let int_lte1_ex = {
  sub_id: Int(LessThanEqualLess),
  term: mk_example("1 <= 2"),
  message: "1 is less than 2, so the expression evaluates to true.",
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
let poly_eq1_ex = {
  sub_id: PolyEqualFalse,
  term: mk_example("1.0 == 2.0"),
  message: "1.0 does not equal 2.0, so the expression evaluates to false.",
};
let poly_eq2_ex = {
  sub_id: PolyEqualTrue,
  term: mk_example("(true, \"str\") == (true, \"str\")"),
  message: "(true, \"str\") is equal to (true, \"str\"), so the expression evaluates to true.",
};
let poly_neq1_ex = {
  sub_id: PolyNotEqualTrue,
  term: mk_example("[1, 2] != [1, 2, 3]"),
  message: "[1, 2] is not equal to [1, 2, 3], so the expression evaluates to true.",
};
let poly_neq2_ex = {
  sub_id: PolyNotEqualFalse,
  term: mk_example("true != true"),
  message: "true is equal to true, so the expression evaluates to false.",
};
let float_plus_ex = {
  sub_id: Float(Plus),
  term: mk_example("1. +. 2.1"),
  message: "1. added to 2.1 evaluates to 3.1",
};
let float_minus_ex = {
  sub_id: Float(Minus),
  term: mk_example("2. -. 1.1"),
  message: "2. minus 1.1 evaluates to 0.9",
};
let float_times_ex = {
  sub_id: Float(Times),
  term: mk_example("1. *. 2.2"),
  message: "1 multiplied by 2.2 evaluates to 2.2.",
};
let float_power_ex = {
  sub_id: Float(Power),
  term: mk_example("2. **. 4."),
  message: "2. raised to 4. evaluates to 16.",
};
let float_divide_ex = {
  sub_id: Float(Divide),
  term: mk_example("4.2 /. 2.1"),
  message: "4.2 divided by 2.1 evaluates to 2.",
};
let float_lt1_ex = {
  sub_id: Float(LessThanTrue),
  term: mk_example("1. <. 2.1"),
  message: "1. is less than 2.1, so the expression evaluates to true.",
};
let float_lt2_ex = {
  sub_id: Float(LessThanFalse),
  term: mk_example("4. <. 3.1"),
  message: "4. is less not less than 3.1, so the expression evaluates to false.",
};
let float_lte1_ex = {
  sub_id: Float(LessThanEqualLess),
  term: mk_example("1. <=. 2.1"),
  message: "1. is less than 2.1, so the expression evaluates to true.",
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
  sub_id: FloatEqualFalse,
  term: mk_example("1. ==. 2."),
  message: "1. does not equal 2., so the expression evaluates to false.",
};
let float_eq2_ex = {
  sub_id: FloatEqualTrue,
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
  message: "The left operand evaluates to true, so evaluate the right operand. Since the right operand also evaluates to true, the whole expression evaluates to true.",
};
let bool_or1_ex = {
  sub_id: OrFalse,
  term: mk_example("false \\/ 2 < 1"),
  message: "The left operand evaluates to false, so evaluate the right operand. Since the right operand also evaluates to false, the whole expression evaluates to false.",
};
let bool_or2_ex = {
  sub_id: OrTrue,
  term: mk_example("3 < 4 \\/ false"),
  message: "The left operand evaluates to true, so the right operand is not evaluated. The whole expression evaluates to true.",
};
let unop_exp_coloring_ids =
    (sf_exp_id: Id.t, ~exp_id: Id.t): list((Id.t, Id.t)) => [
  (sf_exp_id, exp_id),
];
/* Takes the explanation as a format *literal*, so the number of `%s`
   placeholders is checked against the supplied ids at compile time. */
let unop_explanation = (~exp_id: Id.t, fmt): string =>
  Printf.sprintf(fmt, Id.to_string(exp_id));
let e = exp("e");
let bool_unary_not_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) =>
  unop_exp_coloring_ids(Piece.id(e), ~exp_id);
let bool_unary_not_exp = (~exp_id: Id.t): form => {
  id: UnOpExp(Bool(Not)),
  syntactic_form: [unary_not(), e],
  colorings: bool_unary_not_exp_coloring_ids(~exp_id),
  expandable_id: None,
  explanation:
    unop_explanation(
      ~exp_id,
      "Performs boolean negation of the [*operand*](%s).",
    ),
  examples: [],
};
let e = exp("e");
let int_unary_minus_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) =>
  unop_exp_coloring_ids(Piece.id(e), ~exp_id);
let int_unary_minus_exp = (~exp_id: Id.t): form => {
  id: UnOpExp(Int(Minus)),
  syntactic_form: [unary_minus(), e],
  colorings: int_unary_minus_exp_coloring_ids(~exp_id),
  expandable_id: None,
  explanation:
    unop_explanation(
      ~exp_id,
      "Performs integer negation of the [*operand*](%s).",
    ),
  examples: [int_unary_minus_ex],
};
let binop_exp_coloring_ids =
    (sf_left_id: Id.t, sf_right_id: Id.t, ~left_id: Id.t, ~right_id: Id.t)
    : list((Id.t, Id.t)) => {
  [(sf_left_id, left_id), (sf_right_id, right_id)];
};
/* As `unop_explanation`, but for the two-operand forms. */
let binop_explanation = (~left_id: Id.t, ~right_id: Id.t, fmt): string =>
  Printf.sprintf(fmt, Id.to_string(left_id), Id.to_string(right_id));
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_plus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_plus_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(Plus)),
  syntactic_form: [exp1, space(), plus(), space(), exp2],
  colorings: int_plus_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the sum of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [int_plus_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_minus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_minus_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(Minus)),
  syntactic_form: [exp1, space(), minus(), space(), exp2],
  colorings: int_minus_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the difference of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [int_minus_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_times_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_times_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(Times)),
  syntactic_form: [exp1, space(), times(), space(), exp2],
  colorings: int_times_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the product of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [int_times_ex],
};
let int_power_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_power_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(Power)),
  syntactic_form: [exp1, space(), power(), space(), exp2],
  colorings: int_power_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the result of raising [*left*](%s) to the [*right*](%s).",
    ),
  examples: [int_power_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_divide_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_divide_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(Divide)),
  syntactic_form: [exp1, space(), divide(), space(), exp2],
  colorings: int_divide_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the quotient of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [int_divide_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_lt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_lt_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(LessThan)),
  syntactic_form: [exp1, space(), lt(), space(), exp2],
  colorings: int_lt_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is less than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [int_lt1_ex, int_lt2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_lte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_lte_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(LessThanOrEqual)),
  syntactic_form: [exp1, space(), lte(), space(), exp2],
  colorings: int_lte_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is less than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [int_lte1_ex, int_lte2_ex, int_lte3_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_gt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_gt_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(GreaterThan)),
  syntactic_form: [exp1, space(), gt(), space(), exp2],
  colorings: int_gt_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is greater than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [int_gt1_ex, int_gt2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let int_gte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let int_gte_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Int(GreaterThanOrEqual)),
  syntactic_form: [exp1, space(), gte(), space(), exp2],
  colorings: int_gte_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is greater than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [int_gte1_ex, int_gte2_ex, int_gte3_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let poly_eq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let poly_eq_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Poly(Equals)),
  syntactic_form: [exp1, space(), equals(), space(), exp2],
  colorings: poly_eq_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Performs a structural comparison. If the [*left operand*](%s) is equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.",
    ),
  examples: [poly_eq1_ex, poly_eq2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let poly_neq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let poly_neq_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Poly(NotEquals)),
  syntactic_form: [exp1, space(), not_equals(), space(), exp2],
  colorings: poly_neq_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Performs a structural comparison. If the [*left operand*](%s) is not equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.",
    ),
  examples: [poly_neq1_ex, poly_neq2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_plus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_plus_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(Plus)),
  syntactic_form: [exp1, space(), fplus(), space(), exp2],
  colorings: float_plus_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the sum of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [float_plus_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_minus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_minus_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(Minus)),
  syntactic_form: [exp1, space(), fminus(), space(), exp2],
  colorings: float_minus_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the difference of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [float_minus_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_times_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_times_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(Times)),
  syntactic_form: [exp1, space(), ftimes(), space(), exp2],
  colorings: float_times_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the product of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [float_times_ex],
};
let float_power_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_power_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(Power)),
  syntactic_form: [exp1, space(), fpower(), space(), exp2],
  colorings: float_power_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the result of raising [*left*](%s) to the [*right*](%s).",
    ),
  examples: [float_power_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_divide_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_divide_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(Divide)),
  syntactic_form: [exp1, space(), fdivide(), space(), exp2],
  colorings: float_divide_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Gives the quotient of the [*left*](%s) and [*right*](%s) operands.",
    ),
  examples: [float_divide_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_lt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_lt_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(LessThan)),
  syntactic_form: [exp1, space(), flt(), space(), exp2],
  colorings: float_lt_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is less than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [float_lt1_ex, float_lt2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_lte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_lte_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(LessThanOrEqual)),
  syntactic_form: [exp1, space(), flte(), space(), exp2],
  colorings: float_lte_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is less than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [float_lte1_ex, float_lte2_ex, float_lte3_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_gt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_gt_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(GreaterThan)),
  syntactic_form: [exp1, space(), fgt(), space(), exp2],
  colorings: float_gt_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is greater than the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [float_gt1_ex, float_gt2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_gte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_gte_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(GreaterThanOrEqual)),
  syntactic_form: [exp1, space(), fgte(), space(), exp2],
  colorings: float_gte_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is greater than or equal to the [*right operand*](%s), evaluates to `true`. Otherwise evaluates to `false`.",
    ),
  examples: [float_gte1_ex, float_gte2_ex, float_gte3_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_eq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_eq_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(Equals)),
  syntactic_form: [exp1, space(), fequals(), space(), exp2],
  colorings: float_eq_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.",
    ),
  examples: [float_eq1_ex, float_eq2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let float_neq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let float_neq_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Float(NotEquals)),
  syntactic_form: [exp1, space(), fnot_equals(), space(), exp2],
  colorings: float_neq_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) is not equal to the [*right operand*](%s), evaluates to `true`. Otherwise, evaluates to `false`.",
    ),
  examples: [],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let bool_and_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let bool_and_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Bool(And)),
  syntactic_form: [exp1, space(), logical_and(), space(), exp2],
  colorings: bool_and_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) evaluates to `true`, evaluate the [*right operand*](%s). If that also evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
    ),
  examples: [bool_and1_ex, bool_and2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let bool_or_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
// TODO Some of the examples are evaluating weirdly and can't type the || in the editor
let bool_or_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(Bool(Or)),
  syntactic_form: [exp1, space(), logical_or(), space(), exp2],
  colorings: bool_or_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "If the [*left operand*](%s) evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluate the [*right operand*](%s). If that evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
    ),
  examples: [bool_or1_ex, bool_or2_ex],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let str_concat_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  binop_exp_coloring_ids(
    Piece.id(exp1),
    Piece.id(exp2),
    ~left_id,
    ~right_id,
  );
let str_concat_exp = (~left_id: Id.t, ~right_id: Id.t): form => {
  id: BinOpExp(String(Concat)),
  syntactic_form: [exp1, space(), sconcat(), space(), exp2],
  colorings: str_concat_exp_coloring_ids(~left_id, ~right_id),
  expandable_id: None,
  explanation:
    binop_explanation(
      ~left_id,
      ~right_id,
      "Returns the concatenation of the [*left operand*](%s) and the [*right operand*](%s),",
    ),
  examples: [],
};

let bool_un_not = (~exp_id: Id.t): group =>
  singleton(bool_unary_not_exp(~exp_id));

let int_un_minus = (~exp_id: Id.t): group =>
  singleton(int_unary_minus_exp(~exp_id));

let int_plus = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_plus_exp(~left_id, ~right_id));

let int_minus = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_minus_exp(~left_id, ~right_id));

let int_times = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_times_exp(~left_id, ~right_id));

let int_power = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_power_exp(~left_id, ~right_id));

let int_divide = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_divide_exp(~left_id, ~right_id));

let int_less_than = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_lt_exp(~left_id, ~right_id));

let int_less_than_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_lte_exp(~left_id, ~right_id));

let int_greater_than = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_gt_exp(~left_id, ~right_id));

let int_greater_than_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(int_gte_exp(~left_id, ~right_id));

let float_plus = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_plus_exp(~left_id, ~right_id));

let float_minus = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_minus_exp(~left_id, ~right_id));

let float_times = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_times_exp(~left_id, ~right_id));

let float_power = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_power_exp(~left_id, ~right_id));

let float_divide = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_divide_exp(~left_id, ~right_id));

let float_less_than = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_lt_exp(~left_id, ~right_id));

let float_less_than_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_lte_exp(~left_id, ~right_id));

let float_greater_than = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_gt_exp(~left_id, ~right_id));

let float_greater_than_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_gte_exp(~left_id, ~right_id));

let float_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_eq_exp(~left_id, ~right_id));

let float_not_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(float_neq_exp(~left_id, ~right_id));

let bool_and = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(bool_and_exp(~left_id, ~right_id));

let bool_or = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(bool_or_exp(~left_id, ~right_id));

let string_concat = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(str_concat_exp(~left_id, ~right_id));

let poly_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(poly_eq_exp(~left_id, ~right_id));

let poly_not_equal = (~left_id: Id.t, ~right_id: Id.t): group =>
  singleton(poly_neq_exp(~left_id, ~right_id));
