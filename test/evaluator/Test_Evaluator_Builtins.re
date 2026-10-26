open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;
open Exp;

/* The error a builtin call leaves: the call itself, with its evaluated
   argument, in a hole. */
let call_error = (name, arg, err) =>
  dynamic_error_hole(ap(Forward, builtin_fun(name), arg), err);

let tests = (
  "Evaluator.Builtins",
  [
    test_case("Builtin Function application", `Quick, () =>
      evaluation_test(
        "float_of_int(1)",
        float(1.0),
        ap(Forward, var("float_of_int"), int(1)),
      )
    ),
    test_case("Multi-arg builtin with ascription", `Quick, () =>
      evaluation_test(
        {|string_compare(("Hello", "World"):(?, ?))|},
        BuiltinsADT.Ord.lt,
        ap(
          Forward,
          builtin_fun("string_compare"),
          asc(
            tuple([string("Hello"), string("World")]),
            Typ.(prod([Typ.unknown(Internal), Typ.unknown(Internal)])),
          ),
        ),
      )
    ),
    test_case("String_concat builtin", `Quick, () => {
      parse_and_evaluate_test(
        {|"hazel hello world"|},
        {|string_join(" ", ["hazel", "hello", "world"])|},
      )
    }),
    test_case("Multi arg builtin ascription", `Quick, () =>
      evaluation_test(
        {|string_compare(("Hello": ?, "World": ?):(?, ?))|},
        BuiltinsADT.Ord.lt,
        ap(
          Forward,
          builtin_fun("string_compare"),
          asc(
            tuple([
              asc(string("Hello"), Typ.unknown(Internal)),
              asc(string("World"), Typ.unknown(Internal)),
            ]),
            Typ.(prod([Typ.unknown(Internal), Typ.unknown(Internal)])),
          ),
        ),
      )
    ),
    test_case("string_escaped", `Quick, () =>
      parse_and_evaluate_test({|"\\hello"|}, {|string_escaped("\hello")|})
    ),
    test_case("string_uppercase", `Quick, () =>
      parse_and_evaluate_test({|"HELLO"|}, {|string_uppercase("hello")|})
    ),
    test_case("string_lowercase", `Quick, () =>
      parse_and_evaluate_test({|"hello"|}, {|string_lowercase("HELLO")|})
    ),
    test_case("string_capitalize", `Quick, () =>
      parse_and_evaluate_test({|"Hello"|}, {|string_capitalize("hello")|})
    ),
    test_case("string_uncapitalize", `Quick, () =>
      parse_and_evaluate_test({|"hello"|}, {|string_uncapitalize("Hello")|})
    ),
    test_case("string_match true", `Quick, () =>
      evaluation_test(
        {|string_match(("hazel", "hazel"))|},
        bool(true),
        ap(
          Forward,
          builtin_fun("string_match"),
          tuple([string("hazel"), string("hazel")]),
        ),
      )
    ),
    test_case("string_match false", `Quick, () =>
      evaluation_test(
        {|string_match(("hazel", "world"))|},
        bool(false),
        ap(
          Forward,
          builtin_fun("string_match"),
          tuple([string("hazel"), string("world")]),
        ),
      )
    ),
    test_case("string_replace", `Quick, () =>
      evaluation_test(
        {|string_replace(("ha+zel", "haazelhzel", "world"))|},
        string("worldworld"),
        ap(
          Forward,
          builtin_fun("string_replace"),
          tuple([string("ha*zel"), string("hazelhzel"), string("world")]),
        ),
      )
    ),
    test_case("string_search found", `Quick, () =>
      evaluation_test(
        {|string_search(("h.+z", "hazel", 0))|},
        int(0),
        ap(
          Forward,
          builtin_fun("string_search"),
          tuple([string("h.+z"), string("hazel"), int(0)]),
        ),
      )
    ),
    test_case("string_search not found", `Quick, () =>
      evaluation_test(
        {|string_search(("foo", "hazel", 0))|},
        int(-1),
        ap(
          Forward,
          builtin_fun("string_search"),
          tuple([string("foo"), string("hazel"), int(0)]),
        ),
      )
    ),
    test_case(
      "Modulo by zero is DivideByZero",
      `Quick,
      () => {
        evaluation_test(
          "int_mod",
          call_error("int_mod", tuple([int(5), int(0)]), DivideByZero),
          elaborate(parse_exp("int_mod(5, 0)")),
        );
        evaluation_test(
          "sint_mod",
          call_error("sint_mod", tuple([sint(5), sint(0)]), DivideByZero),
          elaborate(parse_exp("sint_mod(5, 0)")),
        );
        evaluation_test(
          "nat_mod",
          call_error(
            "nat_mod",
            tuple([nat(Bigint.of_int(5)), nat(Bigint.zero)]),
            DivideByZero,
          ),
          elaborate(parse_exp("nat_mod(5, 0)")),
        );
        evaluation_test(
          "float_mod",
          call_error(
            "float_mod",
            tuple([float(5.), float(0.)]),
            DivideByZero,
          ),
          elaborate(parse_exp("float_mod(5., 0.)")),
        );
      },
    ),
    test_case(
      "Modulo by a nonzero divisor",
      `Quick,
      () => {
        evaluation_test(
          "int_mod",
          int(1),
          elaborate(parse_exp("int_mod(7, 2)")),
        );
        evaluation_test(
          "sint_mod",
          sint(1),
          elaborate(parse_exp("sint_mod(7, 2)")),
        );
        evaluation_test(
          "nat_mod",
          nat(Bigint.one),
          elaborate(parse_exp("nat_mod(7, 2)")),
        );
        evaluation_test(
          "float_mod",
          float(1.5),
          elaborate(parse_exp("float_mod(7.5, 2.)")),
        );
      },
    ),
    test_case(
      "Modulo by zero inside larger expressions, as for /",
      `Quick,
      () => {
        let div_error =
          dynamic_error_hole(
            bin_op(Int(Divide), int(5), int(0)),
            DivideByZero,
          );
        let mod_error =
          call_error("int_mod", tuple([int(5), int(0)]), DivideByZero);
        evaluation_test(
          "/ in a tuple",
          tuple([div_error, int(2)]),
          elaborate(parse_exp("(5 / 0, 1 + 1)")),
        );
        evaluation_test(
          "int_mod in a tuple",
          tuple([mod_error, int(2)]),
          elaborate(parse_exp("(int_mod(5, 0), 1 + 1)")),
        );
        evaluation_test(
          "/ under an operator",
          bin_op(Int(Plus), div_error, int(1)),
          elaborate(parse_exp("5 / 0 + 1")),
        );
        evaluation_test(
          "int_mod under an operator",
          bin_op(Int(Plus), mod_error, int(1)),
          elaborate(parse_exp("int_mod(5, 0) + 1")),
        );
        evaluation_test(
          "int_mod through a function",
          mod_error,
          elaborate(parse_exp("let f = fun x -> int_mod(x, 0) in f(5)")),
        );
      },
    ),
    /* Records the current out-of-range result, whose call shows only the
       string argument; this is existing behavior, not a chosen display. */
    test_case("string_sub out of range is IndexOutOfBounds", `Quick, () =>
      evaluation_test(
        "string_sub",
        call_error("string_sub", string("ab"), IndexOutOfBounds),
        elaborate(parse_exp({|string_sub("ab", 5, 1)|})),
      )
    ),
  ],
);
