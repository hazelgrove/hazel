open Semantics.Core;

/* a z-expression + it's type + the current metavar generator */
type t = ((ZExp.t, HTyp.t), MetaVar.gen);

/* empty model is the empty hole */
let u_gen0: MetaVar.gen = MetaVar.new_gen;

let (u, u_gen1) = MetaVar.next u_gen0;

let empty: t = ((ZExp.CursorE (HExp.EmptyHole u), HTyp.Hole), u_gen1);

let empty_erasure = HExp.EmptyHole u;

/* convenient type synonyms */
type ms = React.signal t; /* reactive signal */

type mf = step::React.step? => t => unit; /* update function */

type es = React.signal HExp.t; /* derivative reactive signal that only updates when the underlying erasure changes (i.e. not on movement actions) */

type ef = step::React.step? => HExp.t => unit;

exception InvalidAction;

let new_model () => {
  let (ms, mf) = React.S.create empty;
  let (es, ef) = React.S.create empty_erasure;
  let do_action action =>
    switch (Action.performSyn () Ctx.empty action (React.S.value ms)) {
    | Some ((ze, ty), ugen) =>
      mf ((ze, ty), ugen);
      switch action {
      | Action.Move _
      | Action.MoveTo _ => ()
      | _ => ef (ZExp.erase ze)
      }
    | None => raise InvalidAction
    };
  (ms, es, do_action)
};

type mt = (ms, es, Action.t => unit);
