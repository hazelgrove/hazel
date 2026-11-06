open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Reset-numeric-cast-library: Int-to-Rational through the existing
 * constructor `rat` (frozen contract Reset-numeric-cast-library-contract:
 * rat(x, 1) is the only value cast; literal re-kinding is not a cast).
 *
 * `library` is byte-identical to test/libraries/numeric-casts/int-to-rat.hz
 * (reviewers: compare sha256). Every case is a CLOSED instance pinned
 * through parser, statics and evaluator; nothing here states or proves a
 * universal homomorphism law, and there is no Real conversion. Int
 * operands and Int arithmetic are bound OUTSIDE `use Rational`, so the
 * Rational side really receives Int values through int_to_rat.
 * Observations are printed before assertions. */

let library = {|# Numeric casts: Int to Rational with the existing constructor rat. #
# int_to_rat(n) is the exact Rational n, spelled rat(n, 1). It is an #
# ordinary local function, not a builtin; there is no rat_of_int. #
# Literal re-kinding (use Rational, expected type) is not a cast: it #
# only changes how literals and operators are read. #
let int_to_rat : Int -> Rational = fun n -> rat(n, 1) in
|};

let rat_value = (n: int, d: int) =>
  switch (Util.Rational.make(Bigint.of_int(n), Bigint.of_int(d))) {
  | Some(r) => IdTagged.FreshGrammar.Exp.basic(Rat(r))
  | None => Alcotest.fail("zero denominator in expected value")
  };

type obs = {
  static_errors: int,
  ty: string,
  value: string,
};

/* Parse library ++ body, run statics, read the root type, evaluate. */
let observe = (~label, body: string): obs => {
  let src = library ++ body;
  let term = parse_exp(src);
  let (statics, elab) = Test_ProofMap.statics_and_elab(term);
  let (v, _) = Evaluator.evaluate(~statics, ~env=Builtins.env_init, elab);
  let o = {
    static_errors: List.length(Statics.Map.error_ids(statics)),
    ty: Typ.pretty_print(elaborated_type(statics, term)),
    value: Test_ProofMap.print_exp(v),
  };
  print_endline(
    "OBSERVE "
    ++ label
    ++ "\nbody: "
    ++ body
    ++ "\nstatic errors: "
    ++ string_of_int(o.static_errors)
    ++ "\ntype: "
    ++ o.ty
    ++ "\nvalue: "
    ++ o.value
    ++ "\nEND OBSERVE",
  );
  o;
};

let check_valid = (o: obs) =>
  check(int, "valid program: no static errors", 0, o.static_errors);

let check_true = (~label, body) => {
  let o = observe(~label, body);
  check_valid(o);
  check(string, "type", "Bool", o.ty);
  evaluation_test(
    label,
    IdTagged.FreshGrammar.Exp.bool(true),
    elaborate(parse_exp(library ++ body)),
  );
};

let ints = [(-7), (-1), 0, 1, 12];
let lit = n => n < 0 ? "(" ++ string_of_int(n) ++ ")" : string_of_int(n);

/* Closed + - * and == instance for one Int pair, operands bound outside
   `use Rational`. */
let ops_body = (a, b) =>
  "let a = "
  ++ lit(a)
  ++ " in let b = "
  ++ lit(b)
  ++ " in let s = a + b in let d = a - b in let p = a * b in let e = a == b in "
  ++ "use Rational in "
  ++ "int_to_rat(a) + int_to_rat(b) == int_to_rat(s) && "
  ++ "int_to_rat(a) - int_to_rat(b) == int_to_rat(d) && "
  ++ "int_to_rat(a) * int_to_rat(b) == int_to_rat(p) && "
  ++ "(if int_to_rat(a) == int_to_rat(b) then e else (if e then false else true))";

let tests = (
  "NumericCastLibrary",
  [
    test_case(
      "library alone is valid; int_to_rat has type Int -> Rational",
      `Quick,
      () => {
        let o = observe(~label="definition", "int_to_rat");
        check_valid(o);
        check(string, "type", "Int -> Rational", o.ty);
      },
    ),
    test_case(
      "int_to_rat(n) is the exact Rational n (values pinned)",
      `Quick,
      () =>
        List.iter(
          n => {
            let body = "int_to_rat(" ++ lit(n) ++ ")";
            let o = observe(~label="value " ++ string_of_int(n), body);
            check_valid(o);
            check(string, "type", "Rational", o.ty);
            evaluation_test(
              "int_to_rat(" ++ string_of_int(n) ++ ") evaluates to n/1",
              rat_value(n, 1),
              elaborate(parse_exp(library ++ body)),
            );
          },
          ints,
        ),
    ),
    test_case(
      "int_to_rat(n) agrees with the re-kinded literal (use Rational in n)",
      `Quick,
      () =>
        List.iter(
          n =>
            check_true(
              ~label="literal " ++ string_of_int(n),
              "int_to_rat(" ++ lit(n) ++ ") == (use Rational in " ++ lit(n) ++ ")",
            ),
          ints,
        ),
    ),
    test_case(
      "closed + - * and equality instances (negative, zero, positive)",
      `Quick,
      () =>
        List.iter(
          ((a, b)) =>
            check_true(
              ~label="ops " ++ string_of_int(a) ++ " " ++ string_of_int(b),
              ops_body(a, b),
            ),
          [((-7), 12), (12, (-7)), (0, (-1)), ((-1), 0), (1, 1), ((-7), (-7)), (0, 0), (12, 1)],
        ),
    ),
    test_case(
      "negatives: wrong type, no generated converter, distinct values",
      `Quick,
      () => {
        let pinned = observe(~label="ascribed Rational", "(int_to_rat(3) : Rational)");
        check_valid(pinned);
        let wrong = observe(~label="ascribed Int", "(int_to_rat(3) : Int)");
        check(bool, "int_to_rat(3) : Int is a static error", true, wrong.static_errors > 0);
        let gen = observe(~label="rat_of_int", "rat_of_int(3)");
        check(bool, "rat_of_int is unbound", true, gen.static_errors > 0);
        /* run 1: `int_to_rat(use Rational in 3)` has 0 static errors and
           value 3 / 1 -- the parameter's expected Int re-kinds the bare
           literal, so no Rational value is passed. A Rational-typed
           variable is the real negative. */
        let rat_arg =
          observe(~label="Rational argument", "let q : Rational = rat(1, 2) in int_to_rat(q)");
        check(bool, "a Rational argument is a static error", true, rat_arg.static_errors > 0);
        check_true(~label="distinct", "if int_to_rat(2) == int_to_rat(3) then false else true");
        check_true(~label="not a fraction", "if int_to_rat(1) == rat(1, 2) then false else true");
      },
    ),
  ],
);
