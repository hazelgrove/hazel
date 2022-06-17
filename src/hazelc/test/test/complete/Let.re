let test = Case.test_with_eval;

let%test_unit "let annotated bool lit true" =
  test("let x : Bool = true in x");
let%test_unit "let annotated bool lit false" =
  test("let x : Bool = false in x");

let%test_unit "let annotated int lit" = test("let x : Int = 0 in x");

let%test_unit "let annotated float lit" = test("let x : Float = 5. in x");

let%test_unit "let annotated int plus" = test("let x : Int = 0 + 1 in x");
let%test_unit "plus let annotated int" = test("let x : Int = 0 in x + 1");
