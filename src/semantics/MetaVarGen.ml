(* A simple metavariable generator *)
type t = MetaVar.t
let init = 0
let next x = let n = x+1 in x,n
