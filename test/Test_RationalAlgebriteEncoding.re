open Alcotest;

module Enc = Web.RationalAlgebriteEncoding;

let rat = (n: string, d: string): Util.Rational.t =>
  switch (Util.Rational.make(Bigint.of_string(n), Bigint.of_string(d))) {
  | Some(r) => r
  | None => Alcotest.fail("zero denominator")
  };

let text = (name, expected, n, d) =>
  test_case(name, `Quick, () =>
    check(string, n ++ "/" ++ d, expected, Enc.rational(rat(n, d)))
  );

let tests = (
  "RationalAlgebriteEncoding",
  [
    text("non-whole", "(1/2)", "1", "2"),
    text("read off the canonical value", "(1/2)", "2", "4"),
    text("negative", "(-3/4)", "-3", "4"),
    text("negative denominator canonicalised", "(-1/2)", "1", "-2"),
    text("zero", "(0/1)", "0", "5"),
    text("whole", "(5/1)", "5", "1"),
    text(
      "large exact integers",
      "(1267650600228229401496703205377/3)",
      "1267650600228229401496703205377",
      "3",
    ),
    text(
      "large exact whole",
      "(-340282366920938463463374607431768211456/1)",
      "-340282366920938463463374607431768211456",
      "1",
    ),
    test_case("atoms: only Rational", `Quick, () => {
      check(option(string), "Rat", Some("(2/3)"), Enc.atom(Rat(rat("2", "3"))));
      check(option(string), "Int", None, Enc.atom(Int(Bigint.of_int(2))));
      check(option(string), "Float", None, Enc.atom(Float(0.5)));
    }),
    test_case("binary operators", `Quick, () => {
      check(option(string), "power", Some("^"), Enc.bin_op(Rat(Power)));
      check(option(string), "plus", Some("+"), Enc.bin_op(Rat(Plus)));
      check(option(string), "minus", Some("-"), Enc.bin_op(Rat(Minus)));
      check(option(string), "times", Some("*"), Enc.bin_op(Rat(Times)));
      check(option(string), "divide", Some("/"), Enc.bin_op(Rat(Divide)));
      check(option(string), "Int power", None, Enc.bin_op(Int(Power)));
    }),
    test_case("unary minus", `Quick, () => {
      check(option(string), "Rat", Some("-"), Enc.un_op(Rat(Minus)));
      check(option(string), "Int", None, Enc.un_op(Int(Minus)));
    }),
  ],
);
