open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = (
  "Statics.Void",
  [
    fully_consistent_typecheck(
      "Void absurd eliminator",
      {|
let diverge : () -> Void =
  fun () -> diverge()
in
let absurd : Void -> Int =
  fun v -> case v end
in
absurd(diverge())
      |},
      Some(int()),
    ),
  ],
);
