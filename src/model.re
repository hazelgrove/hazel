open Semantics.Core;

/* a z-expression + it's type + the current metavar generator */
type t = ((ZExp.t, HTyp.t), MetaVar.gen);

/* empty model is the empty hole */
let u_gen0: MetaVar.gen = MetaVar.new_gen;

let (u, u_gen1) = MetaVar.next u_gen0;

let empty: t = ((ZExp.CursorE (HExp.EmptyHole u), HTyp.Hole), u_gen1);

/* convenient type synonyms */
type rs = React.signal t; /* reactive signal */

type rf = step::React.step? => t => unit; /* update function */

type rp = (rs, rf); /* pair of rs and rf */
