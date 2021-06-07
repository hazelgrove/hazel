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
  is_focused: bool,
};

let get_edit_state = (p: t('e)): 'e => p.edit_state;

module type EDIT_STATE = {
  [@deriving sexp]
  type t;
  [@deriving sexp]
  type decoration_paths;

  let get_path: t => CursorPath.t;

  let get_doc: (~settings: Settings.t, t) => Pretty.Doc.t(UHAnnot.t);
  let get_decoration_paths: (t, bool) => UHDecorationPaths.t;
  let perform_edit_action: (Action.t, t) => t;
  let get_cursor_info: t => CursorInfo.t;
};

module EditState_Exp = {
  [@deriving sexp]
  type t = Statics.edit_state;
  [@deriving sexp]
  type decoration_paths = UHDecorationPaths.t;

  let get_zexp = edit_state => {
    let (ze, _, _) = edit_state;
    ze;
  };

  let get_ugen = ((_, _, u)) => u;

  let erase = Memo.general(~cache_size_bound=1000, ZExp.erase);
  let get_uhexp = edit_state => edit_state |> get_zexp |> erase;

  let get_path = edit_state => edit_state |> get_zexp |> CursorPath_Exp.of_z;

  let get_doc = (~settings: Settings.t, edit_state: t) => {
    TimeUtil.measure_time(
      "Program.get_doc",
      settings.performance.measure && settings.performance.program_get_doc,
      () => {
      Lazy.force(
        UHDoc_Exp.mk,
        ~memoize=settings.memoize_doc,
        ~enforce_inline=false,
        get_uhexp(edit_state),
      )
    });
  };

  let cursor_info =
    Memo.general(
      ~cache_size_bound=1000,
      CursorInfo_Exp.syn_cursor_info(Contexts.empty),
    );
  let get_cursor_info = edit_state => {
    edit_state
    |> get_zexp
    |> cursor_info
    |> OptUtil.get(() => raise(MissingCursorInfo));
  };

  let get_err_holes_decoration_paths =
      (e: UHExp.t): (list(CursorPath.steps), list(CursorPath.steps)) => {
    CursorPath_Exp.holes(e, [], [])
    |> List.filter_map((CursorPath.{sort, steps}) =>
         switch (sort) {
         | TypHole => None
         | PatHole(_, shape)
         | ExpHole(_, shape) =>
           switch (shape) {
           | Empty => None
           | VarErr
           | TypeErr => Some((shape, steps))
           }
         }
       )
    |> List.partition(
         fun
         | (CursorPath.TypeErr, _) => true
         | (_var_err, _) => false,
       )
    |> TupleUtil.map2(List.map(snd));
  };

  let get_decoration_paths =
      (edit_state: t, is_focused: bool): UHDecorationPaths.t => {
    let current_term = is_focused ? Some(get_path(edit_state)) : None;
    let (err_holes, var_err_holes) =
      edit_state |> get_uhexp |> get_err_holes_decoration_paths;
    let var_uses =
      switch (get_cursor_info(edit_state)) {
      | {uses: Some(uses), _} => uses
      | _ => []
      };
    {current_term, err_holes, var_uses, var_err_holes};
  };

  let expand =
    Memo.general(
      ~cache_size_bound=1000,
      Elaborator_Exp.syn_elab(Contexts.empty, Delta.empty),
    );
  let get_expansion = (edit_state: t): DHExp.t =>
    switch (edit_state |> get_uhexp |> expand) {
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
    | Succeeded(new_edit_state) =>
      let (ze, ty, u_gen) = new_edit_state;
      if (UHExp.is_complete(ZExp.erase(ze))) {
        (ze, ty, MetaVarGen.init);
      } else {
        (ze, ty, u_gen);
      };
    };
  };

  let move_to_hole = (u, (ze, _, _): t) => {
    let holes = CursorPath_Exp.holes(ZExp.erase(ze), [], []);
    switch (CursorPath_common.steps_to_hole(holes, u)) {
    | None => raise(HoleNotFound)
    | Some(hole_steps) =>
      let e = ZExp.erase(ze);
      switch (CursorPath_Exp.of_steps(hole_steps, e)) {
      | None => raise(HoleNotFound)
      | Some(hole_path) => Action.MoveTo(hole_path)
      };
    };
  };

  let cursor_on_exp_hole_ =
    Memo.general(~cache_size_bound=1000, ZExp.cursor_on_EmptyHole);
  let cursor_on_exp_hole = edit_state =>
    edit_state |> get_zexp |> cursor_on_exp_hole_;

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

  let mk: (~width: int, ~is_focused: bool=?, edit_state) => t;
  let focus: t => t;
  let blur: t => t;
  let get_steps: t => CursorPath.steps;

  let get_layout: (~settings: Settings.t, t) => UHLayout.t;
  let get_measured_layout: (~settings: Settings.t, t) => UHMeasuredLayout.t;

  let get_caret_position:
    (~settings: Settings.t, t) => Pretty.MeasuredPosition.t;

  let get_decoration_paths: t => UHDecorationPaths.t;
  let move_via_click:
    (~settings: Settings.t, MeasuredPosition.t, t) => (t, Action.t);
  let move_via_key: (~settings: Settings.t, MoveKey.t, t) => (t, Action.t);
};

module Make = (EditState: EDIT_STATE) : (S with type edit_state = EditState.t) => {
  type edit_state = EditState.t;
  type nonrec t = t(edit_state);

  let mk = (~width: int, ~is_focused=false, edit_state: 'e): t => {
    width,
    edit_state,
    start_col_of_vertical_movement: None,
    is_focused,
  };

  let focus = program => {...program, is_focused: true};
  let blur = program => {...program, is_focused: false};

  let put_edit_state = (edit_state, program) => {...program, edit_state};

  let get_steps = (program: t) => {
    let (steps, _) = program.edit_state |> EditState.get_path;
    steps;
  };

  let get_layout = (~settings: Settings.t, program: t) => {
    let doc = EditState.get_doc(~settings, program.edit_state);
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
    let path = EditState.get_path(program.edit_state);
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

  let get_decoration_paths = (program: t): UHDecorationPaths.t => {
    EditState.get_decoration_paths(program.edit_state, program.is_focused);
  };

  let perform_edit_action = (a, program: t): t => {
    let new_edit_state = EditState.perform_edit_action(a, program.edit_state);
    program |> put_edit_state(new_edit_state);
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
      program
      |> focus
      |> clear_start_col
      |> perform_edit_action(MoveTo(path));
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
