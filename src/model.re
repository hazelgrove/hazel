open Semantics.Core;

/* a z-expression + it's type */
type t = (ZExp.t, HTyp.t);

/* empty model is the empty hole */
let empty = (ZExp.CursorE HExp.EmptyHole, HTyp.Hole);

/* convenient type synonyms */
type rs = React.signal t; /* reactive signal */

type rf = step::React.step? => t => unit; /* update function */

type rp = (rs, rf); /* pair of rs and rf */
