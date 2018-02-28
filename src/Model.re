open Semantics.Core;

/* a z-expression + it's type + the current metavar generator */
type t = ((ZExp.t, HTyp.t), MetaVar.gen);

let u_gen0: MetaVar.gen = MetaVar.new_gen;

let (u, u_gen1) = MetaVar.next u_gen0;

let empty_ze = ZExp.CursorE Before (UHExp.Tm NotInHole (UHExp.EmptyHole u));

let empty: t = ((empty_ze, HTyp.Hole), u_gen1);

let empty_erasure = ZExp.erase empty_ze;

/* convenient type synonyms */
type ms = React.signal t; /* reactive signal */

type mf = step::React.step? => t => unit; /* update function */

type es = React.signal UHExp.t; /* derivative reactive signal that only updates when the underlying erasure changes (i.e. not on movement actions) */

type ef = step::React.step? => UHExp.t => unit;

type cursor_info_rs = React.signal ZExp.cursor_info;

exception InvalidAction;

exception MissingCursorInfo;

let new_model () => {
  let (ms, mf) = React.S.create empty;
  let (es, ef) = React.S.create empty_erasure;
  let cursor_info_rs =
    React.S.l1
      (
        fun ((ze, _), _) =>
          switch (ZExp.syn_cursor_info () Ctx.empty ze) {
          | Some cursor_info => cursor_info
          | None => raise MissingCursorInfo
          }
      )
      ms;
  let do_action action =>
    switch (Action.performSyn () Ctx.empty action (React.S.value ms)) {
    | Some ((ze, ty), ugen) =>
      mf ((ze, ty), ugen);
      switch action {
      | Action.MoveTo _ => ()
      | _ => ef (ZExp.erase ze)
      }
    | None => raise InvalidAction
    };
  (ms, es, cursor_info_rs, do_action)
};

type mt = (ms, es, cursor_info_rs, Action.t => unit);
