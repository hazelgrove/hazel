let test = Test.register_test_with_eval;

let () = test("bool lit true", [], "true");
let () = test("bool lit false", [], "false");

let () = test("int lit 0", [], "0");
let () = test("int lit 5", [], "5");

let () = test("float lit 0.0", [], "0.0");
let () = test("float lit 5.5", [], "5.5");

let () = test("list lit nil", [], "[]");
let () = test("list lit cons", [], "true::[]");
let () = test("list lit cons cons", [], "true::false::[]");
