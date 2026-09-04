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
/* Raised for livelits whose expansion depends on a runtime that the test
   environment cannot provide. */
exception Skip_livelit;

let tests = (
  "Evaluator.Livelit",
  [
    test_case("Ensure evaluation of livelit is as expected", `Quick, () => {
      List.iter(
        (livelit: LivelitCtx.raw_livelit) =>
          try({
            let model = livelit.model_default;
            let expected_eval =
              switch (livelit.name) {
              | "slider" => sint(50)
              | "emotion" => string("neutral")
              | "js" => string("")
              /* The fumola livelit expands by observing an external Fumola
                 runtime reached through `window.fumola`. There is no such
                 runtime under the node test runner, so its expansion here is
                 always the empty hole it degrades to, which says nothing about
                 the livelit itself. Its actual round-trip behaviour -- and the
                 instance-id, reload and duplication semantics -- are covered by
                 the Rust tests in the Fumola repo, crates/fumola_wasm. */
              | "fumola_thunk"
              | "fumola_editor" => raise(Skip_livelit)
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
          }) {
          | Skip_livelit => ()
          },
        Livelit.livelits,
      )
    }),
  ],
);
