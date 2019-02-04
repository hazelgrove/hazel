(* Metavariables, a.k.a. hole names *)
module MetaVar =
 struct
  type t = int
  let rec eq = eqb
 end
