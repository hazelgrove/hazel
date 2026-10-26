open Alcotest;
open Language;

/* R5 domain subunit: Rational domain conditions of the conservative
   domain scan (DomainConditions.scan). Each case states the exact typed
   conditions the scan must emit for one expression, built directly as a
   term (no parser, no statics), and where a condition is closed, its
   value under ordinary evaluation. Integral and Float behaviour is
   pinned as a control. These are conditions the gates then discharge;
   they say nothing about Rational algebra beyond domain errors. */

module R = Util.Rational;

let e = (t: Exp.term): Exp.t => Exp.fresh(t);
let rat = (n: int, d: int): Exp.t =>
  switch (R.make(Bigint.of_int(n), Bigint.of_int(d))) {
  | Some(r) => e(Atom(Rat(r)))
  | None => Alcotest.fail("bad rational literal")
  };
let int = (n: int): Exp.t => e(Atom(Int(Bigint.of_int(n))));
let var = (x: string): Exp.t => e(Var(x));
let rat0 = () => e(Atom(Rat(R.zero)));
let int0 = () => int(0);

let bin = (op: Operators.op_bin, a, b) => e(BinOp(op, a, b));
let rdiv = (a, b) => bin(Rat(Divide), a, b);
let rpow = (a, b) => bin(Rat(Power), a, b);
let call = (f: string, args: list(Exp.t)) =>
  e(Ap(Forward, e(BuiltinFun(f)), e(Tuple(args))));

/* The exact typed conditions. */
let neq = (b, zero) => bin(Poly(NotEquals), b, zero);
let rat_nonzero = b => neq(b, rat0());
let pow_cond = (base, ex) =>
  bin(
    Bool(Or),
    neq(base, rat0()),
    bin(Int(GreaterThanOrEqual), ex, int0()),
  );

let show_list = (es: list(Exp.t)) =>
  "[" ++ String.concat("; ", List.map(Exp.show, es)) ++ "]";

let conditions_are = (msg, expected: list(Exp.t), exp: Exp.t) => {
  let got = DomainConditions.scan(exp);
  let same =
    List.length(got) == List.length(expected)
    && List.for_all2(Exp.fast_equal, got, expected);
  if (!same) {
    Alcotest.fail(
      msg ++ ": expected " ++ show_list(expected) ++ "\n got " ++ show_list(got),
    );
  };
};

/* Closed conditions evaluate to the stated Bool. */
let evaluates_to = (msg, expected: bool, cond: Exp.t) => {
  let (v, _) = Evaluator.evaluate(~env=Builtins.env_init, cond);
  switch (Exp.term_of(v)) {
  | Atom(Bool(b)) => check(bool, msg, expected, b)
  | _ => Alcotest.fail(msg ++ ": not a Bool value: " ++ Exp.show(v))
  };
};

let division_tests = [
  test_case("Rat divide by a literal zero: denominator != 0, false", `Quick, () => {
    conditions_are("1/2 / 0", [rat_nonzero(rat(0, 1))], rdiv(rat(1, 2), rat(0, 1)));
    evaluates_to("0 != 0", false, rat_nonzero(rat(0, 1)));
  }),
  test_case("Rat divide by a nonzero literal: the condition is emitted and true", `Quick, () => {
    conditions_are("1/2 / 3/4", [rat_nonzero(rat(3, 4))], rdiv(rat(1, 2), rat(3, 4)));
    evaluates_to("3/4 != 0", true, rat_nonzero(rat(3, 4)));
  }),
  test_case("Rat divide by a symbolic denominator", `Quick, () =>
    conditions_are("x / y", [rat_nonzero(var("y"))], rdiv(var("x"), var("y")))
  ),
];

let power_tests = [
  test_case("Rat power, zero base, negative exponent: emitted and false", `Quick, () => {
    let c = pow_cond(rat(0, 1), int(-1));
    conditions_are("0 ** -1", [c], rpow(rat(0, 1), int(-1)));
    evaluates_to("0 != 0 || -1 >= 0", false, c);
  }),
  test_case("Rat power, nonzero base, negative exponent: emitted and true", `Quick, () => {
    let c = pow_cond(rat(1, 2), int(-3));
    conditions_are("1/2 ** -3", [c], rpow(rat(1, 2), int(-3)));
    evaluates_to("1/2 != 0 || -3 >= 0", true, c);
  }),
  test_case("Rat power, manifest non-negative exponent: no condition (0 ** 0 defined)", `Quick, () => {
    conditions_are("0 ** 0", [], rpow(rat(0, 1), int(0)));
    conditions_are("0 ** 5", [], rpow(rat(0, 1), int(5)));
    conditions_are("x ** 2", [], rpow(var("x"), int(2)));
  }),
  test_case("Rat power, symbolic base and exponent", `Quick, () =>
    conditions_are("x ** n", [pow_cond(var("x"), var("n"))], rpow(var("x"), var("n")))
  ),
];

let constructor_tests = [
  test_case("rat(n, d): the Int denominator != 0", `Quick, () => {
    conditions_are("rat(1, 0)", [neq(int(0), int0())], call("rat", [int(1), int(0)]));
    conditions_are("rat(1, d)", [neq(var("d"), int0())], call("rat", [int(1), var("d")]));
    evaluates_to("0 != 0 (Int)", false, neq(int(0), int0()));
    evaluates_to("2 != 0 (Int)", true, neq(int(2), int0()));
  }),
  test_case("rat with a non-pair argument: nothing (as the mod builtins)", `Quick, () =>
    conditions_are("rat(p)", [], e(Ap(Forward, e(BuiltinFun("rat")), var("p"))))
  ),
];

let structure_tests = [
  test_case("nested: conditions in pre-order, deduplicated", `Quick, () => {
    /* (x / y) / y : the outer then the inner division, the same y */
    conditions_are(
      "(x / y) / y",
      [rat_nonzero(var("y"))],
      rdiv(rdiv(var("x"), var("y")), var("y")),
    );
    /* rat(1, d) / y : the outer division first, then the constructor */
    conditions_are(
      "rat(1, d) / y",
      [rat_nonzero(var("y")), neq(var("d"), int0())],
      rdiv(call("rat", [int(1), var("d")]), var("y")),
    );
  }),
  test_case("function bodies are not scanned (existing altitude)", `Quick, () =>
    conditions_are(
      "fun z -> z / 0",
      [],
      e(Fun(Pat.fresh(Var("z")), rdiv(var("z"), rat(0, 1)), None, None)),
    )
  ),
  test_case("controls: Int divide unchanged, Float divide emits nothing", `Quick, () => {
    conditions_are(
      "x / y (Int)",
      [neq(var("y"), int0())],
      bin(Int(Divide), var("x"), var("y")),
    );
    conditions_are("x /. y", [], bin(Float(Divide), var("x"), var("y")));
    conditions_are(
      "x ** n (Int)",
      [bin(Int(GreaterThanOrEqual), var("n"), int0())],
      bin(Int(Power), var("x"), var("n")),
    );
    conditions_are(
      "int_mod(a, 0)",
      [neq(int(0), int0())],
      call("int_mod", [var("a"), int(0)]),
    );
  }),
];

let tests = (
  "RationalDomainConditions",
  division_tests @ power_tests @ constructor_tests @ structure_tests,
);
