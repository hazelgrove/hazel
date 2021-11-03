/**  Invalid Input codes:
     0 = out of fuel
     1 = free or invalid variable
     2 = ap invalid boxed function val
     3 = boxed value not a int literal 2
     4 = boxed value not a int literal 1
     5 = bad pattern match
     6 = Cast BV Hole Ground
     7 = boxed value not a float literal 1
     8 = boxed value not a float literal 2
     9 = builtin function name was not found
     10 = builtin function arguments are wrong */

[@deriving sexp]
type t =
  | InvalidInput(int)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);
