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
    CursorInfo.Exp.syn_cursor_info((
      VarCtx.empty,
      Livelits.initial_livelit_ctx,
    )),
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
    Dynamics.Exp.syn_expand(
      (VarCtx.empty, Livelits.initial_livelit_ctx),
      Delta.empty,
    ),
  );
let get_expansion = (program: t): DHExp.t =>
  switch (program |> get_uhexp |> _expand) {
  | DoesNotExpand => raise(DoesNotExpand)
  | Expands(d, _, _) => d
  };

exception InvalidInput;
let _evaluate = {
  Memo.general(~cache_size_bound=1000, Dynamics.Evaluator.evaluate);
};
let get_result = (program: t): Result.t => {
  switch (program |> get_expansion |> _evaluate) {
  | InvalidInput(_) => raise(InvalidInput)
  | BoxedValue(d) =>
    let (d_renumbered, hii, llii) =
      Dynamics.Exp.renumber(
        [],
        HoleInstanceInfo.empty,
        LivelitInstanceInfo.empty,
        d,
      );
    (d_renumbered, hii, llii, BoxedValue(d_renumbered));
  | Indet(d) =>
    let (d_renumbered, hii, llii) =
      Dynamics.Exp.renumber(
        [],
        HoleInstanceInfo.empty,
        LivelitInstanceInfo.empty,
        d,
      );
    (d_renumbered, hii, llii, Indet(d_renumbered));
  };
};

exception FailedAction;
exception CursorEscaped;
let perform_edit_action = (a, program) => {
  let edit_state = program |> get_edit_state;
  switch (
    Action.Exp.syn_perform(
      (VarCtx.empty, Livelits.initial_livelit_ctx),
      a,
      edit_state,
    )
  ) {
  | Failed => raise(FailedAction)
  | CursorEscaped(_) => raise(CursorEscaped)
  | Succeeded(new_edit_state) => program |> put_edit_state(new_edit_state)
  };
};

exception NodeNotFound;
let move_to_node = (kind, u, program) => {
  let (ze, _, _) = program |> get_edit_state;
  let holes = CursorPath.Exp.holes(ZExp.erase(ze), [], []);
  switch (CursorPath.steps_to_hole(holes, kind, u)) {
  | None => raise(NodeNotFound)
  | Some(hole_steps) =>
    program |> perform_edit_action(MoveToBefore(hole_steps))
  };
};

let _doc =
  Memo.general(~cache_size_bound=1000, ((llii, e)) =>
    UHDoc.Exp.mk(
      ~enforce_inline=false,
      ~ctx=Livelits.initial_livelit_view_ctx,
      ~llii,
      e,
    )
  );
let get_doc = program => {
  let e = program |> get_uhexp;
  let (_, _, llii, _) = program |> get_result;
  _doc((llii, e));
};

let _cursor_on_inst =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_on_inst);
let cursor_on_inst = program => program |> get_zexp |> _cursor_on_inst;

let get_layout = program => {
  let width = program |> get_width;
  let (doc, splice_docs) = program |> get_doc;
  let layout =
    doc
    |> Pretty.LayoutOfDoc.layout_of_doc(~width, ~pos=0)
    |> OptUtil.get(() =>
         failwith(__LOC__ ++ ": unimplemented on layout failure")
       );
  let splice_layouts =
    layout
    |> UHLayout.fold(
         ~linebreak=MetaVarMap.empty,
         ~text=_ => MetaVarMap.empty,
         ~align=ls => ls,
         ~cat=(ls1, ls2) => ls1 @ ls2,
         ~annot=
           (pos, annot, ls) =>
             switch (annot) {
             | LivelitView({llu, _}) =>
               let layouts =
                 MetaVarMap.lookup(splice_docs, llu)
                 |> Option.get
                 |> SpliceMap.ApMap.map(doc =>
                      doc
                      |> Pretty.LayoutOfDoc.layout_of_doc(
                           ~width=width - pos.indent,
                           ~pos=0,
                         )
                      |> OptUtil.get(() =>
                           failwith(
                             __LOC__ ++ ": unimplemented on layout failure",
                           )
                         )
                    );
               MetaVarMap.extend_unique(ls, (llu, layouts));
             | _ => ls
             },
       );
  (layout, splice_layouts);
};

let decorate_caret = (path, l) =>
  l
  |> UHLayout.find_and_decorate_caret(~path)
  |> OptUtil.get(() => failwith(__LOC__ ++ ": could not find caret"));
let decorate_cursor = (steps, l) =>
  l
  |> UHLayout.find_and_decorate_cursor(~steps)
  |> OptUtil.get(() => failwith(__LOC__ ++ ": could not find cursor"));
let decorate_var_uses = (ci: CursorInfo.t, l) =>
  switch (ci.uses) {
  | None => l
  | Some(uses) =>
    uses
    |> List.fold_left(
         (l, use) =>
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
  |> CursorMap.of_layout
  |> (
    fun
    | (_, None) => failwith(__LOC__ ++ ": no cursor found")
    | (map, Some(z)) => (map, z)
  );
};

let get_cursor_map = program => program |> get_cursor_map_z |> fst;

let move_via_click = (opt_splice, row_col, program) => {
  let cmap = {
    let (cmap, splice_cmaps) = program |> get_cursor_map;
    switch (opt_splice) {
    | None => cmap
    | Some((u, splice_name)) =>
      splice_cmaps |> SpliceMap.get_splice(u, splice_name)
    };
  };
  let (_, rev_path) = cmap |> CursorMap.find_nearest_within_row(row_col);
  let path = CursorPath.rev(rev_path);
  program |> focus |> clear_start_col |> perform_edit_action(MoveTo(path));
};

let move_via_key = (move_key: JSUtil.MoveKey.t, program) => {
  let (cmap, z) = {
    let ((cmap, splice_cmaps), (opt_splice, z)) =
      program |> get_cursor_map_z;
    switch (opt_splice) {
    | None => (cmap, z)
    | Some((u, splice_name)) => (
        splice_cmaps |> SpliceMap.get_splice(u, splice_name),
        z,
      )
    };
  };
  let ((row, col), _) = z;
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
