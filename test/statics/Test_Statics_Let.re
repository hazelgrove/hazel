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
    synthesizes(
      "a recursive binding is bound at the type its definition has with the pattern at its own type",
      {|let f = fun x -> f in f|},
      Some(arrow(unknown(Internal), unknown(Internal))),
    ),
    fully_consistent_typecheck(
      "a recursive binding checks against an annotation its unrolling would not",
      {|let f = fun x -> f in let y : Int -> Int = f in y|},
      Some(arrow(int(), int())),
    ),
  ],
);
