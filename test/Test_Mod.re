open Alcotest;
open Language;

module F = IdTagged.FreshGrammar;

/* Module items as dynamics sees them: a pending binding of either form
   shadows an evaluated one for the items after it. */
let member_tests = {
  let evaluated_m = Mod.fresh(ModVal("M", F.Exp.var("v0")));
  let stuck = F.Mod.mod_let(F.Pat.var("a"), F.Exp.empty_hole());
  let member_name = (items, name) =>
    switch (Mod.member(items, name)) {
    | Some({term: Var(x), _}) => Some(x)
    | Some(_)
    | None => None
    };
  (
    "Mod.member",
    [
      test_case(
        "an evaluated binding before the stuck item is a member", `Quick, () =>
        check(
          option(string),
          "M",
          Some("v0"),
          member_name([evaluated_m, stuck], "M"),
        )
      ),
      test_case(
        "a pending let rebinding the name leaves the member stuck", `Quick, () =>
        check(
          option(string),
          "stuck",
          None,
          member_name(
            [
              evaluated_m,
              stuck,
              F.Mod.mod_let(F.Pat.var("M"), F.Exp.var("v1")),
            ],
            "M",
          ),
        )
      ),
      test_case(
        "a pending module item rebinding the name leaves the member stuck",
        `Quick,
        () =>
        check(
          option(string),
          "stuck",
          None,
          member_name(
            [
              evaluated_m,
              stuck,
              F.Mod.module_mod(F.MPat.var("M"), F.Exp.var("v1")),
            ],
            "M",
          ),
        )
      ),
    ],
  );
};

let bound_names_tests = (
  "Mod.bound_names",
  [
    test_case("the function-definition sugar binds its name only", `Quick, () =>
      check(
        list(string),
        "f",
        ["f"],
        Mod.bound_names(
          F.Mod.mod_let(
            F.Pat.ap(F.Pat.var("f"), F.Pat.var("x")),
            F.Exp.var("x"),
          ),
        ),
      )
    ),
    test_case("a module item binds its name", `Quick, () =>
      check(
        list(string),
        "N",
        ["N"],
        Mod.bound_names(
          F.Mod.module_mod(F.MPat.var("N"), F.Exp.module_([])),
        ),
      )
    ),
  ],
);

let tests = [member_tests, bound_names_tests];
