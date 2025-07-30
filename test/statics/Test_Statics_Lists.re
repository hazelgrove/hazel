open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = [
  inconsistent_typecheck(
    "list cons inconsistent tail",
    {|1::["str"]|} |> parse_exp,
  ),
  fully_consistent_typecheck(
    "list cons consistent tail",
    {|1::[2]|},
    Some(list(int())),
  ),
];
