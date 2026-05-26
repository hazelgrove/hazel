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
  ],
);
