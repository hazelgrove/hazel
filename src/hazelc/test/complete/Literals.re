let test = Case.test_with_eval;

let%test "bool true" = test("true");
let%test "bool false" = test("false");

let%test "int 0" = test("0");
let%test "int 5" = test("5");

let%test "list nil" = test("[]");
let%test "list cons" = test("true::[]");
