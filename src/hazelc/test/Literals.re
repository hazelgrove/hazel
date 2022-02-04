let%test "true" = {
  let d = DHExp.BoolLit(true);
  Compiler.compile(d) == "true";
};

let%test "false" = {
  let d = DHExp.BoolLit(false);
  Compiler.compile(d) == "false";
};
