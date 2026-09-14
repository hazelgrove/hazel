open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = (
  "Statics.Let",
  [
    synthesizes(
      "inexhaustive let synthesizes its body type (#1671)",
      {|let [x] = [1] in 1|},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "exhaustive let synthesizes its body type",
      {|let x = [1] in 1|},
      Some(int()),
    ),
  ],
);
