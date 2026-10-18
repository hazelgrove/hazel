open Alcotest;
open Haz3lcore;
open Language;

/* A capitalized name parses as a constructor; projected (`Text.f`), it
   names the module of that name when one is in scope, even with a
   constructor `Text` in scope too (found by modular-editors' corpus
   against an HTML builtin). */
let error_count = (code: string): int =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => fail("parse failed: " ++ code)
  | Some(z) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let (info_map, _) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );
    List.length(Statics.Map.error_ids(info_map));
  };

let tests = (
  "Constructor shadowing",
  [
    test_case("module shadows a same-named constructor", `Quick, () =>
      check(
        int,
        "no static errors",
        0,
        error_count(
          "type T = + Text + Other in\nmodule Text = {\n  let f = fun n -> \"x\"\n} in\nText.f(0) == \"x\"",
        ),
      )
    ),
    test_case("constructor still resolves without a module", `Quick, () =>
      check(
        int,
        "no static errors",
        0,
        error_count(
          "type T = + Text + Other in\nlet t : T = Text in\nt == Text",
        ),
      )
    ),
    test_case("module without a colliding constructor", `Quick, () =>
      check(
        int,
        "no static errors",
        0,
        error_count(
          "module Foo = {\n  let f = fun n -> \"x\"\n} in\nFoo.f(0) == \"x\"",
        ),
      )
    ),
  ],
);
