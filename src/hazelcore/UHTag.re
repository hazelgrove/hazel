open Sexplib.Std;

[@deriving sexp]
type t =
  | Tag(string)
  | TagHole(MetaVar.t);

let compare = compare;

let eq = (t1: t, t2: t): bool => t1 == t2;

/* helper function for constructing a new tag hole */
let new_TagHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = u_gen |> MetaVarGen.next;
  (TagHole(u), u_gen);
};
