let compile_string = Compiler.compile_string(~opts={expr_only: true});
let compile_dhexp = Compiler.compile_dhexp(~opts={expr_only: true});

let%test "boollit true" = {
  compile_string("true") == Ok("true");
};

let%test "boollit false" = {
  compile_string("false") == Ok("false");
};

let%test "intlit 0" = {
  compile_string("0") == Ok("0");
};

let%test "intlit 5" = {
  compile_string("5") == Ok("5");
};

let%test "floatlit 1.01" = {
  compile_string("1.01") == Ok("1.01");
};

let%test "unit literal" = {
  // TODO: No way to represent triv in text yet
  compile_dhexp(DHExp.Triv) == Ok("void");
};

let%test "list nil literal" = {
  compile_string("[]") == Ok("[]");
};

let%test "list cons literal" = {
  compile_string("true::[]") == Ok("[true, ...[]]");
};
