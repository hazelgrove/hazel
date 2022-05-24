let test = Case.test_with_eval;

let%test_unit "bool true" = test("true");
let%test_unit "bool false" = test("false");

let%test_unit "int 0" = test("0");
let%test_unit "int 5" = test("5");

let%test_unit "float 0.0" = test("0.0");
let%test_unit "float 5.5" = test("5.5");

let%test_unit "list nil" = test("[]");
let%test_unit "list cons" = test("true::[]");
let%test_unit "list cons" = test("true::false::[]");
