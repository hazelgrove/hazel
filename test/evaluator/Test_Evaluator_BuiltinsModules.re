open Alcotest;
open Test_Evaluator_Prelude;

/* The standard library's modules (BuiltinsModules.re). Each member is the
   same implementation the flat name is bound to, so these check the wiring
   rather than the functions: that the signature admits the member, that the
   projection reaches it, and that a member holding a fixpoint still
   unrolls. */
let tests = (
  "Evaluator.BuiltinsModules",
  [
    test_case("List members are the list functions", `Quick, () =>
      parse_and_evaluate_test(
        "(3, [2, 4, 6], [1, 2], 2)",
        {|(List.length([1, 2, 3]),
           List.map([1, 2, 3], fun x -> x * 2),
           List.filter([1, 2, 3], fun x -> x < 3),
           List.nth([1, 2, 3], 1))|},
      )
    ),
    /* A list function is a fixpoint, which a lookup has to unroll: reaching
       one through a module projection must unroll it the same way reaching
       it through its flat name does. */
    test_case("a recursive member unrolls", `Quick, () =>
      parse_and_evaluate_test(
        "6",
        {|List.fold_left([1, 2, 3], fun (acc, x) -> acc + x, 0)|},
      )
    ),
    test_case("a member is the flat function", `Quick, () =>
      parse_and_evaluate_test("true", {|List.length([1]) == length([1])|})
    ),
    test_case("String members", `Quick, () =>
      parse_and_evaluate_test(
        {|(4, "HI", "42", " a ")|},
        {|(String.length("abcd"),
           String.uppercase("hi"),
           String.of_int(42),
           String.sub((" a b ", 0, 3)))|},
      )
    ),
    test_case("Int and Float members", `Quick, () =>
      parse_and_evaluate_test(
        "(1, 12, 3, 3.0, 2.0)",
        {|(Int.mod((7, 3)),
           Int.of_string("12"),
           Int.abs(-3),
           Float.sqrt(9.0),
           Float.floor(2.5))|},
      )
    ),
    test_case("a constant member", `Quick, () =>
      parse_and_evaluate_test("true", {|Float.pi >. 3.14|})
    ),
    test_case("Option and Pair members", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|(Some(3), [1], 1, "a")|},
        {|(Option.map((Some(2), fun x -> x + 1)),
           Option.to_list(Some(1)),
           Pair.fst((1, "a")),
           Pair.snd((1, "a")))|},
      )
    ),
    /* The instances are ordinary modules until a program marks one implicit,
       and then resolution picks them up like any other instance. */
    test_case("a shipped instance used explicitly", `Quick, () =>
      parse_and_evaluate_test({|"3"|}, {|ShowInt.show(3)|})
    ),
    test_case("a shipped instance marked implicit", `Quick, () =>
      parse_and_evaluate_test(
        {|("3", "hi")|},
        {|let implicit A = ShowInt in
          let implicit B = ShowString in
          let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in
          (show(3), show("hi"))|},
      )
    ),
    test_case("an ORD instance decides a comparison", `Quick, () =>
      parse_and_evaluate_test(
        "7",
        {|let implicit O = OrdInt in
          let bigger = fun (implicit O : ORD, a : O.T, b : O.T) ->
            case O.compare((a, b)) | Gt => a | _ => b end in
          bigger(2, 7)|},
      )
    ),
  ],
);
