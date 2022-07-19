let test = Test.register_test_with_eval;

let () = test("let annotated bool lit true", [], "let x : Bool = true in x");
let () =
  test("let annotated bool lit false", [], "let x : Bool = false in x");

let () = test("let annotated int lit", [], "let x : Int = 0 in x");

let () = test("let annotated float lit", [], "let x : Float = 5. in x");

let () = test("let annotated int plus", [], "let x : Int = 0 + 1 in x");
let () = test("plus let annotated int", [], "let x : Int = 0 in x + 1");
