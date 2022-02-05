let%test "boollit true" = {
  Compiler.compile_string("true") == Ok("true");
};

let%test "boollit false" = {
  Compiler.compile_string("false") == Ok("false");
};

let%test "intlit 0" = {
  Compiler.compile_string("0") == Ok("0");
};

let%test "intlit 5" = {
  Compiler.compile_string("5") == Ok("5");
};

let%test "floatlit 1.01" = {
  Compiler.compile_string("1.01") == Ok("1.01");
};

let%test "unit literal" = {
  // TODO: No way to represent triv in text yet
  Compiler.compile_dhexp(DHExp.Triv) == Ok("void");
};

let%test "list nil literal" = {
  // TODO: Not sure how to write list in text
  let d = DHExp.ListNil(HTyp.Bool);
  Compiler.compile_dhexp(d) == Ok("[]");
};

let%test "list cons literal" = {
  // TODO: Not sure how to write list in text
  let d = DHExp.Cons(BoolLit(true), ListNil(Bool));
  Compiler.compile_dhexp(d) == Ok("[true]");
};
