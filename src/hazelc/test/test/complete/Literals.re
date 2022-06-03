open Hazelc_test_util;

let test = Case.test_with_eval;

let%test_unit "bool lit true" = test("true");
let%test_unit "bool lit false" = test("false");

let%test_unit "int lit 0" = test("0");
let%test_unit "int lit 5" = test("5");

let%test_unit "float lit 0.0" = test("0.0");
let%test_unit "float lit 5.5" = test("5.5");

let%test_unit "list lit nil" = test("[]");
let%test_unit "list lit cons" = test("true::[]");
let%test_unit "list lit cons" = test("true::false::[]");
