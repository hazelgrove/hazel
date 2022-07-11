/* Output type of Evaluator.evaluate. A DHExp.t wrapped
   in its final judgment (boxed value or indeterminate).

   Explanation of this syntax: https://stackoverflow.com/a/31209096/2397327
   */
[@deriving sexp]
type t = DHExp.result = | BoxedValue(DHExp.t) | Indet(DHExp.t);

let unbox: t => DHExp.t;
