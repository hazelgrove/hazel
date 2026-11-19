open Alcotest;
open Js_of_ocaml;
open Web;

/* #2591: the rule palette's shadow-root guard compared a JS value against
   OCaml None, so it never fired. shadow_root must map JS null/undefined to
   None and anything else to Some. */
let has_root = obj =>
  Option.is_some(DerivationExerciseMode.NinjaKeys.shadow_root(obj));

let tests = (
  "NinjaKeys",
  [
    test_case("shadowRoot null is None", `Quick, () =>
      check(
        bool,
        "no root",
        false,
        has_root(
          Js.Unsafe.obj([|("shadowRoot", Js.Unsafe.inject(Js.null))|]),
        ),
      )
    ),
    test_case("shadowRoot undefined is None", `Quick, () =>
      check(bool, "no root", false, has_root(Js.Unsafe.obj([||])))
    ),
    test_case("shadowRoot present is Some", `Quick, () =>
      check(
        bool,
        "root",
        true,
        has_root(Js.Unsafe.obj([|("shadowRoot", Js.Unsafe.obj([||]))|])),
      )
    ),
  ],
);
