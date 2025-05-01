open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = [
  fully_consistent_typecheck(
    "nested_sum_constructors",
    {|
case (? : (rec t -> +Z+S(t)))
  | S(S(x)) => 1
  | _ => 2
end
        |},
    Some(int()),
  ),
];
