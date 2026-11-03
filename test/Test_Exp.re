open Alcotest;
open Language;

let exp = testable(Fmt.using(Exp.show, Fmt.string), Exp.fast_equal);

/* `fun y -> y` annotated `t -> t`. */
let annotated_identity = (t: string): Exp.t =>
  IdTagged.FreshGrammar.(
    Exp.fn(
      Pat.var("y"),
      Exp.var("y"),
      Some(Typ.arrow(Typ.var(t), Typ.var(t))),
      None,
    )
  );

let rename_tvar_tests = (
  "Exp.rename_tvar",
  [
    test_case("Exp.rename_tvar renames a function's annotation", `Quick, () =>
      check(
        exp,
        "fun y -> y : T#1 -> T#1",
        annotated_identity("T#1"),
        Exp.rename_tvar("T", "T#1", annotated_identity("T")),
      )
    ),
  ],
);

let tests = [rename_tvar_tests];
