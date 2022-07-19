let test = Test.register_test_with_eval;

// TODO: Test short-circuiting
let () = test("true && true", [], "true && true");
let () = test("true && false", [], "true && false");
let () = test("false && true", [], "false && true");
let () = test("false && false", [], "false && false");

let () = test("true || true", [], "true || true");
let () = test("true || false", [], "true || false");
let () = test("false || true", [], "false || true");
let () = test("false || false", [], "false || false");

let () = test("0 + 0", [], "0 + 0");
let () = test("1 + 2", [], "1 + 2");

let () = test("1 - 0", [], "0 - 0");
let () = test("1 - 2", [], "1 - 2");

let () = test("0 * 0", [], "0 * 0");
let () = test("1 * 2", [], "1 * 2");

let () = test("0 / 1", [], "0 / 1");
let () = test("1 / 1", [], "1 / 1");
let () = test("1 / 2", [], "1 / 2");

let () = test("0 == 0", [], "1 == 2");
let () = test("1 == 1", [], "1 == 2");
let () = test("1 == 2", [], "1 == 2");

let () = test("0 < 0", [], "0 < 0");
let () = test("0 < 1", [], "0 < 1");
let () = test("2 < 1", [], "2 < 1");

let () = test("0 > 0", [], "0 > 0");
let () = test("0 > 1", [], "0 > 1");
let () = test("2 > 1", [], "2 > 1");

let () = test("0. +. 0.", [], "0. +. 0.");
let () = test("1. +. 2.", [], "1. +. 2.");

let () = test("1. -. 0.", [], "0. -. 0.");
let () = test("1. -. 2.", [], "1. -. 2.");

let () = test("0. *. 0.", [], "0. *. 0.");
let () = test("1. *. 2.", [], "1. *. 2.");

let () = test("0. /. 1.", [], "0. /. 1.");
let () = test("1. /. 1.", [], "1. /. 1.");
let () = test("1. /. 2.", [], "1. /. 2.");

let () = test("0. ==. 0.", [], "1. ==. 2.");
let () = test("1. ==. 1.", [], "1. ==. 2.");
let () = test("1. ==. 2.", [], "1. ==. 2.");

let () = test("0. <. 0.", [], "0. <. 0.");
let () = test("0. <. 1.", [], "0. <. 1.");
let () = test("2. <. 1.", [], "2. <. 1.");

let () = test("0. >. 0.", [], "0. >. 0.");
let () = test("0. >. 1.", [], "0. >. 1.");
let () = test("2. >. 1.", [], "2. >. 1.");
