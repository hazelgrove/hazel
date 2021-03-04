open Sexplib.Std;
open OptUtil.Syntax;

module Memo = Core_kernel.Memo;

module MeasuredLayout = Pretty.MeasuredLayout;
module MeasuredPosition = Pretty.MeasuredPosition;

/**
 * - what's the "gap problem" on slide 6? (was it explained in previous slide?)
 * - slide 6, don't use the term "non-trivial"
 * - demo: need better motivation of different types of edits
 */
module EditState = {
  exception InvalidPath;

  [@deriving sexp]
  type focused = {
    path: CursorPath.t,
    window_has_focus: bool,
  };

  [@deriving sexp]
  type t = {
    term: UHExp.t,
    ty: HTyp.t,
    u_gen: MetaVarGen.t,
    focus: option(focused),
  };

  let get_zterm = edit_state => {
    let+ {path, _} = edit_state.focus;
    CursorPath_Exp.follow(path, edit_state.term)
    |> OptUtil.get_or_raise(InvalidPath);
  };

  let focus = edit_state =>
    switch (edit_state.focus) {
    | None =>
      let focused = {
        path: CursorPath_Exp.of_z(ZExp.place_before(edit_state.term)),
        window_has_focus: true,
      };
      {...edit_state, focus: Some(focused)};
    | Some({path, _}) => {
        ...edit_state,
        focus: Some({path, window_has_focus: true}),
      }
    };
  let blur = edit_state => {...edit_state, focus: None};

  let focus_window = edit_state => {
    ...edit_state,
    focus: {
      let+ {path, _} = edit_state.focus;
      {path, window_has_focus: true};
    },
  };
  let blur_window = edit_state => {
    ...edit_state,
    focus: {
      let+ {path, _} = edit_state.focus;
      {path, window_has_focus: false};
    },
  };
};

[@deriving sexp]
type t = {
  edit_state: EditState.t,
  width: int,
  start_col_of_vertical_movement: option(int),
};

let mk = (~width: int, edit_state: EditState.t) => {
  edit_state,
  width,
  start_col_of_vertical_movement: None,
};

let put_start_col = (start_col, program) => {
  ...program,
  start_col_of_vertical_movement: Some(start_col),
};
let clear_start_col = program => {
  ...program,
  start_col_of_vertical_movement: None,
};

let map_edit_state = (f: EditState.t => EditState.t, program) => {
  ...program,
  edit_state: f(program.edit_state),
};

let focus = map_edit_state(EditState.focus);
let blur = map_edit_state(EditState.blur);

let focus_window = map_edit_state(EditState.focus_window);
let blur_window = map_edit_state(EditState.blur_window);

let erase = Memo.general(~cache_size_bound=1000, ZExp.erase);
let get_uhexp = program => program.edit_state.term;

let get_zexp = program => EditState.get_zterm(program.edit_state);

// TODO(d): remove functions like this from Program,
// push into EditState, have clients work directly with EditState
let get_path = program => {
  let+ {path, _} = program.edit_state.focus;
  path;
};

exception MissingCursorInfo;
let cursor_info =
  Memo.general(
    ~cache_size_bound=1000,
    CursorInfo_Exp.syn_cursor_info(Contexts.empty),
  );
let get_cursor_info = (program: t): option(CursorInfo.t) => {
  let+ ze = get_zexp(program);
  cursor_info(ze) |> OptUtil.get(() => raise(MissingCursorInfo));
};

let get_decoration_paths = (program: t): UHDecorationPaths.t => {
  let current_term = get_path(program);
  let (err_holes, var_err_holes) =
    CursorPath_Exp.holes(get_uhexp(program), [], [])
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
  let var_uses =
    switch (get_cursor_info(program)) {
    | Some({uses: Some(uses), _}) => uses
    | _ => []
    };
  {current_term, err_holes, var_uses, var_err_holes};
};

exception DoesNotElaborate;
let expand =
  Memo.general(
    ~cache_size_bound=1000,
    Elaborator_Exp.syn_elab(Contexts.empty, Delta.empty),
  );
let get_expansion = (program: t): DHExp.t =>
  switch (program |> get_uhexp |> expand) {
  | DoesNotElaborate => raise(DoesNotElaborate)
  | Elaborates(d, _, _) => d
  };

exception InvalidInput;

let evaluate = Memo.general(~cache_size_bound=1000, Evaluator.evaluate);
let get_result = (program: t): Result.t =>
  switch (program |> get_expansion |> evaluate) {
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

let get_doc = (~settings: Settings.t, program) => {
  TimeUtil.measure_time(
    "Program.get_doc",
    settings.performance.measure && settings.performance.program_get_doc,
    () => {
    Lazy.force(
      UHDoc_Exp.mk,
      ~memoize=settings.memoize_doc,
      ~enforce_inline=false,
      get_uhexp(program),
    )
  });
};

let get_layout = (~settings: Settings.t, program) => {
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

let get_measured_layout = (~settings: Settings.t, program): UHMeasuredLayout.t => {
  program |> get_layout(~settings) |> UHMeasuredLayout.mk;
};

let get_caret_position =
    (~settings: Settings.t, program): option(MeasuredPosition.t) => {
  let m = get_measured_layout(~settings, program);
  switch (program.edit_state.focus) {
  | Some({window_has_focus, path}) when window_has_focus =>
    let pos =
      UHMeasuredLayout.caret_position_of_path(path, m)
      |> OptUtil.get(() => failwith("could not find caret"));
    Some(pos);
  | _ => None
  };
};

exception FailedAction;
exception CursorEscaped;
let perform_action =
    (~settings, ~move_via: option(MoveInput.t)=?, a: Action.t, program: t) => {
  let performed =
    switch (move_via, get_zexp(program)) {
    | (None | Some(Key(_)), None) => program
    | (Some(Click(_)), None) =>
      // TODO(d) clean up this hack
      switch (a) {
      | MoveTo(path) => {
          ...program,
          edit_state: {
            ...program.edit_state,
            focus: Some({path, window_has_focus: true}),
          },
        }
      | _ => program
      }
    | (_, Some(ze)) =>
      let EditState.{ty, u_gen, _} = program.edit_state;
      switch (Action_Exp.syn_perform(Contexts.empty, a, (ze, ty, u_gen))) {
      | Failed => raise(FailedAction)
      | CursorEscaped(_) => raise(CursorEscaped)
      | Succeeded((ze, ty, u_gen)) =>
        // TODO(d) maybe encapsulate in EditState
        let edit_state =
          EditState.{
            // ensure pointer stability
            term:
              Action.is_movement(a) ? program.edit_state.term : erase(ze),
            ty,
            u_gen: UHExp.is_complete(erase(ze)) ? MetaVarGen.init : u_gen,
            focus:
              Some({path: CursorPath_Exp.of_z(ze), window_has_focus: true}),
          };
        {...program, edit_state};
      };
    };

  let update_start_col: t => t =
    switch (move_via) {
    | None
    | Some(Click(_) | Key(ArrowLeft | ArrowRight | Home | End)) => clear_start_col
    | Some(Key(ArrowUp | ArrowDown)) =>
      switch (
        program.start_col_of_vertical_movement,
        get_caret_position(~settings, program),
      ) {
      | (Some(_), _)
      | (_, None) => (p => p)
      | (None, Some(caret_position)) => put_start_col(caret_position.col)
      }
    };
  update_start_col(performed);
};

exception HoleNotFound;
let move_to_hole = (u, program) => {
  let e = get_uhexp(program);
  let holes = CursorPath_Exp.holes(e, [], []);
  switch (CursorPath_common.steps_to_hole(holes, u)) {
  | None => raise(HoleNotFound)
  | Some(hole_steps) =>
    switch (CursorPath_Exp.of_steps(hole_steps, e)) {
    | None => raise(HoleNotFound)
    | Some(hole_path) => Action.MoveTo(hole_path)
    }
  };
};

let move_to_case_branch = (steps_to_case, branch_index): Action.t => {
  let steps_to_branch = steps_to_case @ [1 + branch_index];
  Action.MoveTo((steps_to_branch, OnDelim(1, After)));
};

let cursor_on_exp_hole_ =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_on_EmptyHole);
let cursor_on_exp_hole = program => {
  let* ze = get_zexp(program);
  cursor_on_exp_hole_(ze);
};

let target_path_of_click_input =
    (~settings: Settings.t, target: MeasuredPosition.t, program): CursorPath.t => {
  let (rev_path, _) =
    program
    |> get_measured_layout(~settings)
    |> UHMeasuredLayout.nearest_path_within_row(target)
    |> OptUtil.get(() => failwith("row with no caret positions"));
  CursorPath.rev(rev_path);
};

let target_path_of_key_input =
    (~settings: Settings.t, move_key: MoveKey.t, program)
    : option(CursorPath.t) => {
  let* caret_position = get_caret_position(~settings, program);
  let from_col =
    switch (program.start_col_of_vertical_movement) {
    | None => caret_position.col
    | Some(col) => col
    };
  let m = get_measured_layout(~settings, program);
  let+ (rev_path, _) =
    switch (move_key) {
    | ArrowUp =>
      let up_target =
        MeasuredPosition.{row: caret_position.row - 1, col: from_col};
      UHMeasuredLayout.nearest_path_within_row(up_target, m);
    | ArrowDown =>
      let down_target =
        MeasuredPosition.{row: caret_position.row + 1, col: from_col};
      UHMeasuredLayout.nearest_path_within_row(down_target, m);
    | ArrowLeft =>
      switch (UHMeasuredLayout.prev_path_within_row(caret_position, m)) {
      | Some(_) as found => found
      | None =>
        caret_position.row > 0
          ? UHMeasuredLayout.last_path_in_row(caret_position.row - 1, m)
          : None
      }
    | ArrowRight =>
      switch (UHMeasuredLayout.next_path_within_row(caret_position, m)) {
      | Some(_) as found => found
      | None =>
        caret_position.row < MeasuredLayout.height(m) - 1
          ? UHMeasuredLayout.first_path_in_row(caret_position.row + 1, m)
          : None
      }
    | Home => UHMeasuredLayout.first_path_in_row(caret_position.row, m)
    | End => UHMeasuredLayout.last_path_in_row(caret_position.row, m)
    };
  CursorPath.rev(rev_path);
};
