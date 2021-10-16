open Sexplib.Std;

module Memo = Core_kernel.Memo;
module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

exception CursorEscaped;
/**
 * Raised when edit state does not elaborate
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state) */
exception DoesNotElaborate;
/**
 * Raised when evaluation fails with the InvalidInput output
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state)
 */
exception InvalidInput;
/**
 * Raised when an attempted edit action does not succeed
 */
exception FailedAction;
exception HoleNotFound;
/**
 * Raised when `CursorInfo_Exp.syn_cursor_info` returns None
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state)
 */
exception MissingCursorInfo;

/**
 * A Hazel program ready for user interaction.
 * Contains, in addition to `Statics.edit_state`,
 * user interface state such as the current width of
 * the editor, whether the editor is focused, etc.
 */
[@deriving sexp]
type t('e) = {
  edit_state: 'e,
  width: int,
  start_col_of_vertical_movement: option(int),
};

let get_edit_state = (p: t('e)): 'e => p.edit_state;

module type EDIT_STATE = {
  [@deriving sexp]
  type t;
  type sort;
  type z_sort;

  let get_zstx: t => z_sort;
  let get_uhstx: t => sort;

  let mk_doc:
    lazy_t((~memoize: bool, ~enforce_inline: bool, sort) => UHDoc.t);
  let perform_edit_action: (Action.t, t) => t;
  let cursor_info: (Contexts.t, z_sort) => option(CursorInfo.t);
  let holes:
    (sort, CursorPath.rev_steps, CursorPath.hole_list) => CursorPath.hole_list;
  let of_steps: (CursorPath.steps, sort) => option(CursorPath.t);
  let of_z: z_sort => CursorPath.t;
};

module EditState_Typ = {
  [@deriving sexp]
  type t = ZTyp.t;
  type sort = UHTyp.t;
  type z_sort = ZTyp.t;

  let get_zstx = edit_state => edit_state;
  let erase = Memo.general(~cache_size_bound=1000, ZTyp.erase);
  let get_uhstx = edit_state => edit_state |> get_zstx |> erase;
  let of_z = CursorPath_Typ.of_z;
  let mk_doc = UHDoc_Typ.mk;
  let holes = CursorPath_Typ.holes;
  let cursor_info = CursorInfo_Typ.cursor_info(~steps=[]);
  let of_steps = CursorPath_Typ.of_steps(~side=Before);

  let perform_edit_action = (a, edit_state: t): t => {
    switch (Action_Typ.perform(a, edit_state)) {
    | Failed => raise(FailedAction)
    | CursorEscaped(_) => raise(CursorEscaped)
    | Succeeded(new_edit_state) => new_edit_state
    };
  };
};
module EditState_Pat = {
  [@deriving sexp]
  type t = (ZPat.t, HTyp.t, MetaVarGen.t);
  type sort = UHPat.t;
  type z_sort = ZPat.t;

  let get_zstx = ((zp, _, _)) => zp;
  let erase = Memo.general(~cache_size_bound=1000, ZPat.erase);
  let get_uhstx = edit_state => edit_state |> get_zstx |> erase;
  let of_z = CursorPath_Pat.of_z;
  let mk_doc = UHDoc_Pat.mk;
  let holes = CursorPath_Pat.holes;
  let cursor_info = (ctx, zp) =>
    switch (CursorInfo_Pat.syn_cursor_info(ctx, zp, ~steps=[])) {
    // TODO (andrew): not sure what this all is
    | None => None
    | Some(CursorNotOnDeferredVarPat(ci)) => Some(ci)
    | Some(CursorOnDeferredVarPat(_, _)) => None
    };
  let of_steps = CursorPath_Pat.of_steps(~side=Before);

  let perform_edit_action = (a, (zp, _, u_gen): t): t => {
    switch (Action_Pat.syn_perform(Contexts.empty, u_gen, a, zp)) {
    | Failed => raise(FailedAction)
    | CursorEscaped(_) => raise(CursorEscaped)
    | Succeeded((zp, ty, _ctx, u_gen)) =>
      if (UHPat.is_complete(ZPat.erase(zp))) {
        (zp, ty, MetaVarGen.init);
      } else {
        (zp, ty, u_gen);
      }
    };
  };

  let mk = (~hty=HTyp.Hole, ~u_gen=MetaVarGen.init, zp) => (zp, hty, u_gen);
};

module EditState_Exp = {
  [@deriving sexp]
  type t = Statics.edit_state;
  type sort = UHExp.t;
  type z_sort = ZExp.t;

  let get_zstx = ((ze, _, _)) => ze;
  let get_ugen = ((_, _, u)) => u;
  let erase = Memo.general(~cache_size_bound=1000, ZExp.erase);
  let get_uhstx = edit_state => edit_state |> get_zstx |> erase;
  let of_z = CursorPath_Exp.of_z;
  let mk_doc = UHDoc_Exp.mk;
  let holes = CursorPath_Exp.holes;
  let cursor_info =
    CursorInfo_Exp.syn_cursor_info(
      ~enclosing_zoperand=None,
      ~steps=[],
      ~enclosing_zopseq=CursorInfo.NoSeq,
    );
  let of_steps = CursorPath_Exp.of_steps(~side=Before);

  let mk = (~hty=HTyp.Hole, ~u_gen=MetaVarGen.init, ze) => (ze, hty, u_gen);
  let expand =
    Memo.general(
      ~cache_size_bound=1000,
      Elaborator_Exp.syn_elab(Contexts.empty, Delta.empty),
    );
  let get_expansion = (edit_state: t): DHExp.t =>
    switch (edit_state |> get_uhstx |> expand) {
    | DoesNotElaborate => raise(DoesNotElaborate)
    | Elaborates(d, _, _) => d
    };

  let evaluate = Memo.general(~cache_size_bound=1000, Evaluator.evaluate);
  let get_result = (edit_state: t): Result.t =>
    switch (edit_state |> get_expansion |> evaluate) {
    | InvalidInput(_) => raise(InvalidInput)
    | BoxedValue(d) =>
      let (d_renumbered, hii) =
        Elaborator_Exp.renumber([], HoleInstanceInfo.empty, d);
      (d_renumbered, hii, BoxedValue(d_renumbered));
    | Indet(d) =>
      let (d_renumbered, hii) =
        Elaborator_Exp.renumber([], HoleInstanceInfo.empty, d);
      (d_renumbered, hii, Indet(d_renumbered));
    };

  let perform_edit_action = (a, edit_state: t): t => {
    switch (Action_Exp.syn_perform(Contexts.empty, a, edit_state)) {
    | Failed => raise(FailedAction)
    | CursorEscaped(_) => raise(CursorEscaped)
    | Succeeded((ze, ty, u_gen)) =>
      if (UHExp.is_complete(ZExp.erase(ze))) {
        (ze, ty, MetaVarGen.init);
      } else {
        (ze, ty, u_gen);
      }
    };
  };

  let cursor_on_exp_hole_ =
    Memo.general(~cache_size_bound=1000, ZExp.cursor_on_EmptyHole);
  let cursor_on_exp_hole = edit_state =>
    edit_state |> get_zstx |> cursor_on_exp_hole_;

  /**
 * `move_to_case_branch(steps, n)` returns an action that moves the cursor to
 * the `n`th branch in case expression found at `steps` (when the user
 * clicks on a branch type in the error message for a case expression with
 * inconsistent branches)
 */
  let move_to_case_branch = (steps_to_case, branch_index): Action.t => {
    let steps_to_branch = steps_to_case @ [1 + branch_index];
    Action.MoveTo((steps_to_branch, OnDelim(1, After)));
  };
};

module type S = {
  type edit_state;
  type nonrec t = t(edit_state);

  let mk: (~width: int, edit_state) => t;

  let get_path: t => CursorPath.t;
  let get_steps: t => CursorPath.steps;

  let get_cursor_info: t => CursorInfo.t;
  let get_layout: (~settings: Settings.t, t) => UHLayout.t;
  let get_measured_layout: (~settings: Settings.t, t) => UHMeasuredLayout.t;

  let get_caret_position:
    (~settings: Settings.t, t) => Pretty.MeasuredPosition.t;

  let get_decoration_paths: (t, bool) => UHDecorationPaths.t;
  //let get_err_holes_decoration_paths:
  //  t => (list(CursorPath.steps), list(CursorPath.steps));
  let move_to_hole: (int, t) => Action.t;
  let move_via_click:
    (~settings: Settings.t, MeasuredPosition.t, t) => (t, Action.t);
  let move_via_key: (~settings: Settings.t, MoveKey.t, t) => (t, Action.t);
};

module Make = (EditState: EDIT_STATE) : (S with type edit_state = EditState.t) => {
  type edit_state = EditState.t;
  type nonrec t = t(edit_state);

  let mk = (~width: int, edit_state: 'e): t => {
    width,
    edit_state,
    start_col_of_vertical_movement: None,
  };

  let put_edit_state = (edit_state, program) => {...program, edit_state};

  let get_path = (program: t) =>
    program |> get_edit_state |> EditState.get_zstx |> EditState.of_z;
  let get_steps = (program: t) => program |> get_path |> fst;

  let get_doc = (~settings: Settings.t, program: t) => {
    TimeUtil.measure_time(
      "Editor.get_doc",
      settings.performance.measure && settings.performance.program_get_doc,
      () => {
      Lazy.force(
        EditState.mk_doc,
        ~memoize=settings.memoize_doc,
        ~enforce_inline=false,
        program |> get_edit_state |> EditState.get_uhstx,
      )
    });
  };

  let get_layout = (~settings: Settings.t, program: t) => {
    let doc = get_doc(~settings, program);
    TimeUtil.measure_time(
      "LayoutOfDoc.layout_of_doc",
      settings.performance.measure
      && settings.performance.layoutOfDoc_layout_of_doc,
      () =>
      Pretty.LayoutOfDoc.layout_of_doc(~width=program.width, ~pos=0, doc)
    )
    |> OptUtil.get(() => failwith("unimplemented: layout failure"));
  };

  let get_measured_layout =
      (~settings: Settings.t, program): UHMeasuredLayout.t => {
    program |> get_layout(~settings) |> UHMeasuredLayout.mk;
  };
  let get_caret_position =
      (~settings: Settings.t, program): MeasuredPosition.t => {
    let m = get_measured_layout(~settings, program);
    let path = get_path(program);
    UHMeasuredLayout.caret_position_of_path(path, m)
    |> OptUtil.get(() => failwith("could not find caret"));
  };

  let put_start_col = (start_col: int, program: t): t => {
    ...program,
    start_col_of_vertical_movement: Some(start_col),
  };
  let clear_start_col = (program: t): t => {
    ...program,
    start_col_of_vertical_movement: None,
  };

  let cursor_info =
    Memo.general(
      ~cache_size_bound=1000,
      EditState.cursor_info(Contexts.empty),
    );
  let get_cursor_info = program => {
    program
    |> get_edit_state
    |> EditState.get_zstx
    |> cursor_info
    |> OptUtil.get(() => raise(MissingCursorInfo));
  };

  let get_decoration_paths =
      (program: t, is_focused: bool): UHDecorationPaths.t => {
    let current_term = is_focused ? Some(get_path(program)) : None;
    let (err_holes, var_err_holes) =
      EditState.holes(
        program |> get_edit_state |> EditState.get_uhstx,
        [],
        [],
      )
      |> List.filter_map(hole_info =>
           switch (CursorPath.get_sort(hole_info)) {
           | TypHole => None
           | PatHole(_, shape)
           | ExpHole(_, shape) =>
             switch (shape) {
             | Empty => None
             | VarErr
             | TypeErr => Some((shape, CursorPath.get_steps(hole_info)))
             }
           }
         )
      |> List.partition(
           fun
           | (CursorPath.TypeErr, _) => true
           | (_var_err, _) => false,
         )
      |> TupleUtil.map2(List.map(snd));
    let var_uses =
      try(
        switch (get_cursor_info(program)) {
        | {uses: Some(uses), _} => uses
        | _ => []
        }
      ) {
      | _ => []
      };
    {current_term, err_holes, var_uses, var_err_holes};
  };

  let perform_edit_action = (a, program: t): t => {
    let new_edit_state = EditState.perform_edit_action(a, program.edit_state);
    program |> put_edit_state(new_edit_state);
  };

  let move_to_hole = (u, program: t) => {
    let s = program |> get_edit_state |> EditState.get_uhstx;
    let holes = EditState.holes(s, [], []);
    switch (CursorPath_common.steps_to_hole(holes, u)) {
    | None => raise(HoleNotFound)
    | Some(hole_steps) =>
      switch (EditState.of_steps(hole_steps, s)) {
      | None => raise(HoleNotFound)
      | Some(hole_path) => Action.MoveTo(hole_path)
      }
    };
  };

  let move_via_click =
      (~settings: Settings.t, target: MeasuredPosition.t, program)
      : (t, Action.t) => {
    let m = get_measured_layout(~settings, program);
    let path =
      UHMeasuredLayout.nearest_path_within_row(target, m)
      |> OptUtil.get(() => failwith("row with no caret positions"))
      |> fst
      |> CursorPath_common.rev;
    let new_program =
      program |> clear_start_col |> perform_edit_action(MoveTo(path));
    (new_program, MoveTo(path));
  };

  let move_via_key =
      (~settings: Settings.t, move_key: MoveKey.t, program): (t, Action.t) => {
    let caret_position = get_caret_position(~settings, program);
    let m = get_measured_layout(~settings, program);
    let (from_col, put_col_on_start) =
      switch (program.start_col_of_vertical_movement) {
      | None =>
        let col = caret_position.col;
        (col, put_start_col(col));
      | Some(col) => (col, (p => p))
      };
    let (new_z, update_start_col) =
      switch (move_key) {
      | ArrowUp =>
        let up_target =
          MeasuredPosition.{row: caret_position.row - 1, col: from_col};
        (
          UHMeasuredLayout.nearest_path_within_row(up_target, m),
          put_col_on_start,
        );
      | ArrowDown =>
        let down_target =
          MeasuredPosition.{row: caret_position.row + 1, col: from_col};
        (
          UHMeasuredLayout.nearest_path_within_row(down_target, m),
          put_col_on_start,
        );
      | ArrowLeft => (
          switch (UHMeasuredLayout.prev_path_within_row(caret_position, m)) {
          | Some(_) as found => found
          | None =>
            caret_position.row > 0
              ? UHMeasuredLayout.last_path_in_row(caret_position.row - 1, m)
              : None
          },
          clear_start_col,
        )
      | ArrowRight => (
          switch (UHMeasuredLayout.next_path_within_row(caret_position, m)) {
          | Some(_) as found => found
          | None =>
            caret_position.row < MeasuredLayout.height(m) - 1
              ? UHMeasuredLayout.first_path_in_row(caret_position.row + 1, m)
              : None
          },
          clear_start_col,
        )
      | Home => (
          UHMeasuredLayout.first_path_in_row(caret_position.row, m),
          clear_start_col,
        )
      | End => (
          UHMeasuredLayout.last_path_in_row(caret_position.row, m),
          clear_start_col,
        )
      };

    switch (new_z) {
    | None => raise(CursorEscaped)
    | Some((rev_path, _)) =>
      let path = CursorPath_common.rev(rev_path);
      let new_program =
        program |> update_start_col |> perform_edit_action(MoveTo(path));
      (new_program, MoveTo(path));
    };
  };
};

[@deriving sexp]
type exp = t(EditState_Exp.t);
module Exp = Make(EditState_Exp);
[@deriving sexp]
type typ = t(EditState_Typ.t);
module Typ = Make(EditState_Typ);
[@deriving sexp]
type pat = t(EditState_Pat.t);
module Pat = Make(EditState_Pat);

let mk_typ_editor = ty => ty |> ZTyp.place_before |> Typ.mk(~width=80);

let get_ty = (editor: typ): HTyp.t =>
  editor |> get_edit_state |> EditState_Typ.get_uhstx |> UHTyp.expand;

let mk_exp_editor = (exp: UHExp.t): exp =>
  exp |> ZExp.place_before |> EditState_Exp.mk |> Exp.mk(~width=80);

let mk_pat_editor = (exp: UHPat.t): pat =>
  exp |> ZPat.place_before |> EditState_Pat.mk |> Pat.mk(~width=80);
