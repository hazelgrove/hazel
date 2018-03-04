open Semantics.Core;

/* a z-expression + it's type + the current metavar generator */
type edit_state = ((ZExp.t, HTyp.t), MetaVar.gen);

let u_gen0: MetaVar.gen = MetaVar.new_gen;

let (u, u_gen1) = MetaVar.next u_gen0;

let empty_ze = ZExp.CursorE Before (UHExp.Tm NotInHole (UHExp.EmptyHole u));

let empty: edit_state = ((empty_ze, HTyp.Hole), u_gen1);

let empty_erasure = ZExp.erase empty_ze;

/* convenient type synonyms */
type edit_state_rs = React.signal edit_state; /* reactive signal */

type e_rs = React.signal UHExp.t; /* derivative reactive signal that only updates when the underlying erasure changes (i.e. not on movement actions) */

type cursor_info_rs = React.signal ZExp.cursor_info;

open Dynamics;

type result_rs =
  React.signal (DHExp.t, DHExp.HoleInstanceInfo.t, Evaluator.result);

type hole_instance_info_rs = React.signal DHExp.HoleInstanceInfo.t;

type selected_instance_rs = React.signal (option DHExp.HoleInstance.t);

type selected_instance_rf =
  step::React.step? => option DHExp.HoleInstance.t => unit;

type monitors = list (React.signal unit);

exception InvalidAction;

exception MissingCursorInfo;

exception DoesNotExpand;

exception InvalidInput;

type t = {
  edit_state_rs, /* changes whenever the edit state changes */
  cursor_info_rs, /* ZExp.cursor_info computed from z_rs */
  e_rs, /* changes only for non-movement actions */
  result_rs, /* computed result */
  selected_instance_rs,
  selected_instance_rf,
  monitors,
  do_action: Action.t => unit /* function to perform actions */
};

let new_model () :t => {
  let (edit_state_rs, edit_state_rf) = React.S.create empty;
  let (e_rs, e_rf) = React.S.create empty_erasure;
  let cursor_info_rs =
    React.S.l1
      (
        fun ((ze, _), _) =>
          switch (ZExp.syn_cursor_info () Ctx.empty ze) {
          | Some cursor_info => cursor_info
          | None => raise MissingCursorInfo
          }
      )
      edit_state_rs;
  let result_rs =
    React.S.l1
      (
        fun e => {
          let expanded = DHExp.syn_expand () Ctx.empty e;
          switch expanded {
          | DHExp.DoesNotExpand => raise DoesNotExpand
          | DHExp.Expands d _ _ =>
            switch (Evaluator.evaluate () d) {
            | Evaluator.InvalidInput n =>
              JSUtil.log ("InvalidInput " ^ string_of_int n);
              raise InvalidInput
            | Evaluator.BoxedValue d =>
              let (d_renumbered, hii) =
                DHExp.renumber () [] DHExp.HoleInstanceInfo.empty d;
              (d_renumbered, hii, Evaluator.BoxedValue d_renumbered)
            | Evaluator.Indet d =>
              let (d_renumbered, hii) =
                DHExp.renumber () [] DHExp.HoleInstanceInfo.empty d;
              (d_renumbered, hii, Evaluator.Indet d_renumbered)
            }
          }
        }
      )
      e_rs;
  let (selected_instance_rs, selected_instance_rf) = React.S.create None;
  /* when cursor is on a hole, set the instance path to the default for that hole */
  let instance_at_cursor_monitor =
    React.S.l2
      (
        fun {ZExp.mode: _, ZExp.form: form, ZExp.ctx: _} (_, hii, _) => {
          let new_path =
            switch form {
            | ZExp.IsHole u =>
              switch (DHExp.HoleInstanceInfo.default_instance hii u) {
              | Some (u, i) as inst => inst
              | None => None
              }
            | _ => None
            };
          selected_instance_rf new_path
        }
      )
      cursor_info_rs
      result_rs;
  /* Keep monitors around in the state to stop them from being garbage collected */
  let monitors = [instance_at_cursor_monitor];
  let do_action action =>
    switch (
      Action.performSyn () Ctx.empty action (React.S.value edit_state_rs)
    ) {
    | Some ((ze, ty), ugen) =>
      edit_state_rf ((ze, ty), ugen);
      switch action {
      | Action.MoveTo _ => ()
      | _ => e_rf (ZExp.erase ze)
      }
    | None => raise InvalidAction
    };
  {
    edit_state_rs,
    cursor_info_rs,
    e_rs,
    result_rs,
    selected_instance_rs,
    selected_instance_rf,
    monitors,
    do_action
  }
};
