/* Output type of Evaluator.evaluate. A DHExp.t wrapped
   in its final judgment (boxed value or indeterminate).
   */
[@deriving sexp]
type t = DHExp.result;

let unbox: t => DHExp.t;
