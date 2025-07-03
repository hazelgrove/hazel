open Alcotest;
open Haz3lcore;
open Language;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;
open Exp;

let exp_to_segment =
  ExpToSegment.(
    exp_to_segment(~settings=Settings.of_core(~inline=true, CoreSettings.on))
  );
let tests = (
  "Evaluator.Livelit",
  [
    test_case("Ensure evaluation of livelit is as expected", `Quick, () => {
      List.iter(
        (livelit: LivelitCtx.raw_livelit) => {
          let model = livelit.model_default;
          let expected_eval =
            switch (livelit.name) {
            | "slider" => sint(50)
            | "emotion" => string("neutral")
            | "js" => string("")
            | _ => Alcotest.fail("Unknown Livelit " ++ livelit.name)
            };

          let model_string =
            switch (model) {
            | {term: Tuple(_), _} =>
              Printer.of_segment(exp_to_segment(model))
            | _ => "(" ++ Printer.of_segment(exp_to_segment(model)) ++ ")"
            };

          parse_and_evaluate_test(
            Printer.of_segment(exp_to_segment(expected_eval)),
            "^" ++ livelit.name ++ model_string,
          );
        },
        Livelit.livelits,
      )
    }),
  ],
);
