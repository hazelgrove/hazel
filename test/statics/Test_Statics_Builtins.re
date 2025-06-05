open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = [
  fully_consistent_typecheck(
    "Tuple extension",
    {|(a=0, 1, b=2) ... (a=1, 3, c=4)|},
    Some(
      prod([
        tup_label(label("a"), int()),
        int(),
        tup_label(label("b"), int()),
        int(),
        tup_label(label("c"), int()),
      ]),
    ),
  ),
];
