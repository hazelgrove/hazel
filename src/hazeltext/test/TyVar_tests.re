let%test "type all holes" =
  Test.test_parse("type ? = ? in let ? : ? = ? in ?");
let%test "type no holes" =
  Test.test_parse("type t = Int in let x : t = 123 in x");

let%test "local type" =
  Test.test_parse(
    "let x = type t = Int in type u = [t] in let y : t = [] in y in y",
  );
