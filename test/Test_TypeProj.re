/* The type projector's model is persisted as a sexp and read back at render
 * time, where ProjectorBase.Cook calls deserialize_m with no guard -- a raise
 * there takes the view down. So the reader has to be total, and it has to
 * understand the shape it used to have: before the model gained a `length` it
 * WAS the bare mode. These pin both. */

open Alcotest;
open Haz3lcore;

let read = (s: string): string =>
  s
  |> Sexplib.Sexp.of_string
  |> TypeProj.M.model_of_sexp
  |> TypeProj.M.sexp_of_model
  |> Sexplib.Sexp.to_string;

/* What the current shape serializes to, for whichever mode. */
let current = (mode: string): string =>
  read("((mode " ++ mode ++ ") (length Auto))");

let tests = (
  "TypeProj.model_of_sexp",
  [
    test_case("a legacy bare-mode model keeps its mode", `Quick, () =>
      List.iter(
        mode =>
          check(
            string,
            "legacy " ++ mode ++ " reads as that mode, unsized",
            current(mode),
            read(mode),
          ),
        ["Expected", "Self", "Dynamic"],
      )
    ),
    test_case("a dragged length survives a round-trip", `Quick, () =>
      check(
        string,
        "Fixed(7) is preserved",
        "((mode Self)(length(Fixed 7)))",
        read("((mode Self) (length (Fixed 7)))"),
      )
    ),
    test_case("an unreadable model falls back instead of raising", `Quick, () =>
      List.iter(
        junk =>
          check(
            string,
            "junk reads as the default: " ++ junk,
            current("Expected"),
            read(junk),
          ),
        ["Nonsense", "((mode Wat) (length Auto))", "((unexpected 1))", "()"],
      )
    ),
  ],
);
