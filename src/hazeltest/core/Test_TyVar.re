open Tezt;
open Tezt.Base;

let register_test = (title, tags, text) =>
  Test.register(~__FILE__, ~title, ~tags=["hazeltext"] @ tags, () =>
    Test_Hazeltext.test_parse(text) ? unit : Test.fail("parse failed!")
  );

let () =
  register_test("type all holes", [], "type ? = ? in let ? : ? = ? in ?");
let () =
  register_test("type no holes", [], "type t = Int in let x : t = 123 in x");
let () =
  register_test(
    "local type",
    [],
    "let x = type t = Int in type u = [t] in let y : t = [] in y in y",
  );
