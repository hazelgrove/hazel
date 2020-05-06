module Memo = Core_kernel.Memo;

type t = {
  edit_state: Statics.edit_state,
  width: int,
  start_col_of_vertical_movement: option(int),
  is_focused: bool,
};

let mk = (~width: int, ~is_focused=false, edit_state: Statics.edit_state): t => {
  width,
  edit_state,
  start_col_of_vertical_movement: None,
  is_focused,
};

let get_width = program => program.width;

let get_start_col = program => program.start_col_of_vertical_movement;
let put_start_col = (start_col, program) => {
  ...program,
  start_col_of_vertical_movement: Some(start_col),
};
let clear_start_col = program => {
  ...program,
  start_col_of_vertical_movement: None,
};

let is_focused = program => program.is_focused;

let focus = program => {...program, is_focused: true};
let blur = program => {...program, is_focused: false};

let get_edit_state = program => program.edit_state;
let put_edit_state = (edit_state, program) => {...program, edit_state};

let get_zexp = program => {
  let (ze, _, _) = program |> get_edit_state;
  ze;
};

let _erase = Memo.general(~cache_size_bound=1000, ZExp.erase);
let get_uhexp = program => program |> get_zexp |> _erase;

let get_path = program => program |> get_zexp |> CursorPath.Exp.of_z;
let get_steps = program => {
  let (steps, _) = program |> get_path;
  steps;
};

let get_u_gen = program => {
  let (_, _, u_gen) = program |> get_edit_state;
  u_gen;
};

exception MissingCursorInfo;
let _cursor_info =
  Memo.general(
    ~cache_size_bound=1000,
    CursorInfo.Exp.syn_cursor_info(Contexts.empty),
  );
let get_cursor_info = (program: t) => {
  program
  |> get_zexp
  |> _cursor_info
  |> OptUtil.get(() => raise(MissingCursorInfo));
};

exception DoesNotExpand;
let _expand =
  Memo.general(
    ~cache_size_bound=1000,
    Dynamics.Exp.syn_expand(Contexts.empty, Delta.empty),
  );
let get_expansion = (program: t): DHExp.t =>
  switch (program |> get_uhexp |> _expand) {
  | DoesNotExpand => raise(DoesNotExpand)
  | Expands(d, _, _) => d
  };

exception InvalidInput;
let _evaluate =
  Memo.general(~cache_size_bound=1000, Dynamics.Evaluator.evaluate);
let get_result = (program: t): Result.t =>
  switch (program |> get_expansion |> _evaluate) {
  | InvalidInput(_) => raise(InvalidInput)
  | BoxedValue(d) =>
    let (d_renumbered, hii) =
      Dynamics.Exp.renumber([], HoleInstanceInfo.empty, d);
    (d_renumbered, hii, BoxedValue(d_renumbered));
  | Indet(d) =>
    let (d_renumbered, hii) =
      Dynamics.Exp.renumber([], HoleInstanceInfo.empty, d);
    (d_renumbered, hii, Indet(d_renumbered));
  };

exception FailedAction;
exception CursorEscaped;
let perform_edit_action = (a, program) => {
  let edit_state = program |> get_edit_state;
  switch (Action.Exp.syn_perform(Contexts.empty, a, edit_state)) {
  | Failed => raise(FailedAction)
  | CursorEscaped(_) => raise(CursorEscaped)
  | Succeeded(new_edit_state) => program |> put_edit_state(new_edit_state)
  };
};

exception HoleNotFound;
let move_to_hole = (u, program) => {
  let (ze, _, _) = program |> get_edit_state;
  let holes = CursorPath.Exp.holes(ZExp.erase(ze), [], []);
  switch (CursorPath.steps_to_hole(holes, u)) {
  | None => raise(HoleNotFound)
  | Some(hole_steps) =>
    program |> perform_edit_action(MoveToBefore(hole_steps))
  };
};

let _doc =
  Memo.general(~cache_size_bound=1000, UHDoc.Exp.mk(~enforce_inline=false));
let get_doc = program => {
  let e = program |> get_uhexp;
  _doc(e);
};

let get_layout = program => {
  let width = program |> get_width;
  program
  |> get_doc
  |> Pretty.LayoutOfDoc.layout_of_doc(~width, ~pos=0)
  |> OptUtil.get(() => failwith("unimplemented: layout failure"));
};

let decorate_caret = (path, l) =>
  l
  |> UHLayout.find_and_decorate_caret(~path)
  |> OptUtil.get(() => failwith(__LOC__ ++ ": could not find caret"));
let decorate_cursor = (steps, l) =>
  l
  |> UHLayout.find_and_decorate_cursor(~steps)
  |> OptUtil.get(() => failwith(__LOC__ ++ ": could not find cursor"));
let decorate_var_uses = (ci: CursorInfo.t, l: UHLayout.t): UHLayout.t =>
  switch (ci.typed) {
  | PatAnaVar(_, _, _, _, [_, ..._] as uses, _)
  | PatSynVar(_, _, _, _, [_, ..._] as uses, _) =>
    uses
    |> List.fold_left(
         (l: UHLayout.t, use) =>
           l
           |> UHLayout.find_and_decorate_var_use(~steps=use)
           |> OptUtil.get(() => {
                failwith(
                  __LOC__
                  ++ ": could not find var use"
                  ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(use)),
                )
              }),
         l,
       )
  | _ => l
  };

let get_decorated_layout = program => {
  let (steps, _) as path = program |> get_path;
  let ci = program |> get_cursor_info;
  program
  |> get_layout
  |> decorate_caret(path)
  |> decorate_cursor(steps)
  |> decorate_var_uses(ci);
};

let get_cursor_map_z = program => {
  let path = program |> get_path;
  // TODO figure out how to consolidate decoration
  program
  |> get_layout
  |> decorate_caret(path)
  |> CursorMap.mk
  |> (
    fun
    | (_, None) => failwith(__LOC__ ++ ": no cursor found")
    | (map, Some(z)) => (map, z)
  );
};

let get_cursor_map = program => program |> get_cursor_map_z |> fst;

let move_via_click = (row_col, program) => {
  let (_, rev_path) =
    program |> get_cursor_map |> CursorMap.find_nearest_within_row(row_col);
  let path = CursorPath.rev(rev_path);
  program |> focus |> clear_start_col |> perform_edit_action(MoveTo(path));
};

let move_via_key = (move_key: JSUtil.MoveKey.t, program) => {
  let (cmap, ((row, col), _) as z) = program |> get_cursor_map_z;
  let (from_col, put_col_on_start) =
    switch (program |> get_start_col) {
    | None => (col, put_start_col(col))
    | Some(col) => (col, (p => p))
    };
  let (new_z, update_start_col) =
    switch (move_key) {
    | ArrowUp => (
        cmap |> CursorMap.move_up((row, from_col)),
        put_col_on_start,
      )
    | ArrowDown => (
        cmap |> CursorMap.move_down((row, from_col)),
        put_col_on_start,
      )
    | ArrowLeft => (cmap |> CursorMap.move_left(z), clear_start_col)
    | ArrowRight => (cmap |> CursorMap.move_right(z), clear_start_col)
    | Home => (Some(cmap |> CursorMap.move_sol(row)), clear_start_col)
    | End => (Some(cmap |> CursorMap.move_eol(row)), clear_start_col)
    };
  switch (new_z) {
  | None => raise(CursorEscaped)
  | Some((_, rev_path)) =>
    let path = CursorPath.rev(rev_path);
    program |> update_start_col |> perform_edit_action(MoveTo(path));
  };
};

let _cursor_on_exp_hole =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_on_EmptyHole);
let cursor_on_exp_hole = program => program |> get_zexp |> _cursor_on_exp_hole;
