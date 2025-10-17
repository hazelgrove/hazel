open Alcotest;
open Test_Statics_Prelude;
open Language;
open Haz3lcore;

module FreshGrammar = IdTagged.FreshGrammar;

module FMError =
  Grammar.Factory({
    type t = option(ErrorMessage.message);
    let default_value = () => None;
  });

// abanduk: These tests are mostly examples. I haven't gone through and enumerated what tests would be useful
let tests = (
  "Statics.ErrorMessages",
  [
    error_message_tree_test(
      "Free variable error message",
      FMError.Exp.(
        var(
          ~ann=
            Some({
              is_error: true,
              fragments: [Code("x"), Text("not found")],
            }),
          "x",
        )
      ),
    ),
    error_message_tree_test(
      "Successful integer type message",
      FMError.Exp.(
        int(
          ~ann=
            Some({
              is_error: false,
              fragments: [Text(":"), Type(FreshGrammar.Typ.int())],
            }),
          42,
        )
      ),
    ),
    error_message_tree_test(
      "1 + 2",
      FMError.Exp.(
        bin_op(
          Int(Plus),
          int(
            ~ann=
              Some({
                is_error: true,
                fragments: [
                  Text(":"),
                  Type(FreshGrammar.Typ.int()),
                  Text("equals expected type"),
                ],
              }),
            1,
          ),
          int(2),
        )
      ),
    ),
  ],
);
