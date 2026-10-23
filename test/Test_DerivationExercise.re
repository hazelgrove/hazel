open Alcotest;
open Web;

let round_trip = (pos: DerivationExercise.pos) =>
  test_case(
    "pos_of_key inverts key_for_statics: " ++ DerivationExercise.show_pos(pos),
    `Quick,
    () =>
    check(
      string,
      "pos",
      DerivationExercise.show_pos(pos),
      DerivationExercise.show_pos(
        DerivationExercise.pos_of_key(
          DerivationExercise.key_for_statics(pos),
        ),
      ),
    )
  );

let tests = (
  "DerivationExercise",
  [
    round_trip(Prelude),
    round_trip(Setup),
    round_trip(Trees(3, Value)),
    round_trip(Trees(3, Children(1, Children(12, Value)))),
  ],
);
