open Alcotest;
open Language;

/* The Tree Care slide, loaded as the editor loads it: its tree is fed by
   a growth-style splice, built by ^either from two ^range sliders. */

let tests = (
  "TreeCare",
  [
    test_case(
      "the slide checks, and means its census",
      `Quick,
      () => {
        let (m, elab) = Test_Quote.slide("playground/tree-care.hz");
        check(
          list(string),
          "no errors",
          [],
          Test_ExpansionErrors.messages(m),
        );
        let v = Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst;
        check(
          Test_Evaluator_Prelude.dhexp_typ,
          "leaves, branches, scars",
          Test_UserLivelits.run("(3, 2, 0)"),
          v,
        );
      },
    ),
  ],
);
