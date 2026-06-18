open Alcotest;
open Language;

/* Unit tests for Haz3lcore.ConstraintGen: translating core expressions into
 * SMT-LIB2 expression strings. Pure (no statics, no solver), so these run
 * natively and under node. */

module E = IdTagged.FreshGrammar.Exp;
module P = IdTagged.FreshGrammar.Pat;

let smt = Haz3lcore.ConstraintGen.smt_of_exp;

let case = (name, exp, expected) =>
  test_case(name, `Quick, () => check(string, name, expected, smt(exp)));

let x = E.var("x");

let tests = (
  "ConstraintGen",
  [
    case(
      "int comparison",
      E.bin_op(Operators.Int(GreaterThan), x, E.int(5)),
      "(> x 5)",
    ),
    case(
      "boolean and of comparisons",
      E.bin_op(
        Operators.Bool(And),
        E.bin_op(Operators.Int(GreaterThan), x, E.int(5)),
        E.bin_op(Operators.Int(LessThan), x, E.int(10)),
      ),
      "(and (> x 5) (< x 10))",
    ),
    case(
      "negation",
      E.un_op(Operators.Bool(Not), E.bool(true)),
      "(not true)",
    ),
    case("unary minus", E.un_op(Operators.Int(Minus), E.int(5)), "(- 5)"),
    case("negative literal", E.int(-3), "(- 3)"),
    case(
      "if as ite",
      E.if_(E.var("b"), E.int(1), E.int(2)),
      "(ite b 1 2)",
    ),
    case(
      "float not-equals",
      E.bin_op(Operators.Float(NotEquals), E.var("f"), E.var("g")),
      "(not (= f g))",
    ),
    case(
      "string concat",
      E.bin_op(Operators.String(Concat), E.string("a"), E.string("b")),
      {|(str.++ "a" "b")|},
    ),
    case(
      "parens are transparent",
      E.parens(E.bin_op(Operators.Int(Plus), E.int(1), E.int(2))),
      "(+ 1 2)",
    ),
    case(
      "tuple equality is component-wise",
      E.bin_op(
        Operators.Poly(Equals),
        E.tuple([x, E.var("y")]),
        E.tuple([E.int(1), E.int(2)]),
      ),
      "(and (= x 1) (= y 2))",
    ),
    case(
      "match desugars to nested ite",
      E.match(
        E.var("n"),
        [
          (P.basic(Atom.Int(Bigint.of_int(0))), E.bool(true)),
          (P.wild(), E.bool(false)),
        ],
      ),
      "(ite (= n 0) true false)",
    ),
  ],
);
