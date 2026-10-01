open Alcotest;
open Haz3lcore;
open Language;

/* the default context is one record per mode, so analyzing the same term
   again hits Statics.mk's memo */
let repeat_hits = () => {
  let term =
    switch (FastParse.of_text(~root=Exp, "let x = 1 in\nx + 1")) {
    | Some(seg) => MakeTerm.go(seg).term
    | None => fail("unparseable")
    };
  let run = () =>
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  let first = run();
  check(bool, "the cached result comes back", true, run() === first);
};

let tests = (
  "StaticsMemo",
  [test_case("a repeat analysis hits the memo", `Quick, repeat_hits)],
);
