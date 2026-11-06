/* The Int sum/product list library (Reset-list-sum-int-library), as
   Hazel source. Ordinary structural recursion over [Int]: no builtin,
   no fold_left, no stated law. Programs that use it append their own
   lets/theorems and a tail. See README.md in this directory for the
   relation to fold_left and the open universal frontier. */

let definitions = {|let sum_int = fun (xs : [Int]) -> case xs | [] => 0 | x :: t => x + sum_int(t) end in
let prod_int = fun (xs : [Int]) -> case xs | [] => 1 | x :: t => x * prod_int(t) end in
|};
