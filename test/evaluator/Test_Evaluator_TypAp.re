open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.TypAp",
  [
    test_case("Explicit polymorphism with type alias", `Quick, () =>
      parse_and_evaluate_test(
        "[5, 3]",
        {|type T=String in
let map = typfun a ->typfun b -> fun f :(a ->b), as : [a] -> let bs : [b] =
  case as
    | [] => []
    | (a :: as) => f(a) :: map@<a>@<b>(f, as)
  end
in bs in

map@<T>@<Int>(fun e -> string_length(e), ["hello","bar"])|},
      )
    ),
    /* Test_Statics_Polymorphism pins the type this result has to agree with. */
    test_case(
      "typfun binder shadows a type alias, so the body keeps `y` (#1717)",
      `Quick,
      () =>
      parse_and_evaluate_test(
        {|typfun y -> (?:y)|},
        {|type y = Int in typfun y -> (? : y)|},
      )
    ),
    /* Implicit polymorphism elaborates to hidden typfuns and @<…>s. */
    test_case("Generalized let used at two types", `Quick, () =>
      parse_and_evaluate_test(
        "(1, true)",
        {|let id = fun x -> x in (id(1), id(true))|},
      )
    ),
    test_case("Explicitly poly function applied without @<>", `Quick, () =>
      parse_and_evaluate_test(
        "3",
        {|let id : poly a -> a -> a = typfun a -> fun x : a -> x in id(3)|},
      )
    ),
    test_case("Generalized recursive map", `Quick, () =>
      parse_and_evaluate_test(
        "([2, 3], [false])",
        {|let map = fun f -> fun xs ->
  case xs
    | [] => []
    | x :: t => f(x) :: map(f)(t)
  end
in (map(fun x -> x + 1)([1, 2]), map(fun b -> !b)([true]))|},
      )
    ),
  ],
);
