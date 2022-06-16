let test = Case.test_with_eval;

// TODO: Test short-circuiting
let%test_unit "true && true" = test("true && true");
let%test_unit "true && false" = test("true && false");
let%test_unit "false && true" = test("false && true");
let%test_unit "false && false" = test("false && false");

let%test_unit "true || true" = test("true || true");
let%test_unit "true || false" = test("true || false");
let%test_unit "false || true" = test("false || true");
let%test_unit "false || false" = test("false || false");

let%test_unit "0 + 0" = test("0 + 0");
let%test_unit "1 + 2" = test("1 + 2");

let%test_unit "1 - 0" = test("0 - 0");
let%test_unit "1 - 2" = test("1 - 2");

let%test_unit "0 * 0" = test("0 * 0");
let%test_unit "1 * 2" = test("1 * 2");

let%test_unit "0 / 1" = test("0 / 1");
let%test_unit "1 / 1" = test("1 / 1");
let%test_unit "1 / 2" = test("1 / 2");

let%test_unit "0 == 0" = test("1 == 2");
let%test_unit "1 == 1" = test("1 == 2");
let%test_unit "1 == 2" = test("1 == 2");

let%test_unit "0 < 0" = test("0 < 0");
let%test_unit "0 < 1" = test("0 < 1");
let%test_unit "2 < 1" = test("2 < 1");

let%test_unit "0 > 0" = test("0 > 0");
let%test_unit "0 > 1" = test("0 > 1");
let%test_unit "2 > 1" = test("2 > 1");

let%test_unit "0. +. 0." = test("0. +. 0.");
let%test_unit "1. +. 2." = test("1. +. 2.");

let%test_unit "1. -. 0." = test("0. -. 0.");
let%test_unit "1. -. 2." = test("1. -. 2.");

let%test_unit "0. *. 0." = test("0. *. 0.");
let%test_unit "1. *. 2." = test("1. *. 2.");

let%test_unit "0. /. 1." = test("0. /. 1.");
let%test_unit "1. /. 1." = test("1. /. 1.");
let%test_unit "1. /. 2." = test("1. /. 2.");

let%test_unit "0. ==. 0." = test("1. ==. 2.");
let%test_unit "1. ==. 1." = test("1. ==. 2.");
let%test_unit "1. ==. 2." = test("1. ==. 2.");

let%test_unit "0. <. 0." = test("0. <. 0.");
let%test_unit "0. <. 1." = test("0. <. 1.");
let%test_unit "2. <. 1." = test("2. <. 1.");

let%test_unit "0. >. 0." = test("0. >. 0.");
let%test_unit "0. >. 1." = test("0. >. 1.");
let%test_unit "2. >. 1." = test("2. >. 1.");
