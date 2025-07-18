open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = [
  fully_consistent_typecheck(
    "Function with unknown param",
    "fun x -> 4 + 5",
    Some(Typ.(arrow(unknown(Ana), int()))),
  ),
  fully_consistent_typecheck(
    "Function with known param",
    "fun x : Int -> 4 + 5",
    Some(arrow(int(), int())),
  ),
  fully_consistent_typecheck(
    "Function with labeled param",
    "fun (a=x) -> 4",
    Some(arrow(prod([tup_label(label("a"), unknown(Ana))]), int())),
  ),
  fully_consistent_typecheck(
    "bifunction",
    "fun x : Int, y: Int -> x + y",
    Some(arrow(prod([int(), int()]), int())),
  ),
  fully_consistent_typecheck(
    "bifunction",
    "fun x : Int, y: Int -> x + y",
    Some(arrow(prod([int(), int()]), int())),
  ),
  fully_consistent_typecheck(
    "function application",
    "float_of_int(1)",
    Some(float()),
  ),
  fully_consistent_typecheck(
    "function deferral",
    "string_sub(\"hello\", 1, _)",
    Some(arrow(int(), string())),
  ),
  fully_consistent_typecheck(
    "Fixpoint in function position",
    {|(fix f : (Int -> Int) -> fun x -> x + 1)(3)|},
    Some(int()),
  ),
];
