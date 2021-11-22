open Sexplib.Std;

module Memo = Core_kernel.Memo;

[@deriving sexp]
type t = {
  edit_state: Statics_common.edit_state,
  width: int,
  start_col_of_vertical_movement: option(int),
  is_focused: bool,
};

let mk =
    (~width: int, ~is_focused=false, edit_state: Statics_common.edit_state): t => {
  width,
  edit_state,
  start_col_of_vertical_movement: None,
  is_focused,
};

let put_start_col = (start_col, program) => {
  ...program,
  start_col_of_vertical_movement: Some(start_col),
};
let clear_start_col = program => {
  ...program,
  start_col_of_vertical_movement: None,
};

let focus = program => {...program, is_focused: true};
let blur = program => {...program, is_focused: false};

let put_edit_state = (edit_state, program) => {...program, edit_state};

let get_zexp = program => {
  let (ze, _, _) = program.edit_state;
  ze;
};

let get_uhexp = program => program |> get_zexp |> ZExp.erase;

let get_path = program => program |> get_zexp |> CursorPath_Exp.of_z;
let get_steps = program => {
  let (steps, _) = program |> get_path;
  steps;
};

let get_u_gen = program => {
  let (_, _, u_gen) = program.edit_state;
  u_gen;
};

exception MissingCursorInfo;
let get_cursor_info = (program: t) => {
  program
  |> get_zexp
  |> CursorInfo_Exp.syn_cursor_info(Contexts.empty)
  |> OptUtil.get(() => raise(MissingCursorInfo));
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

let get_result = (program: t): Result.t =>
  switch (program |> get_expansion |> Evaluator.evaluate) {
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

exception FailedAction;
exception CursorEscaped;
let perform_edit_action = (a, program) => {
  let edit_state = program.edit_state;
  switch (Action_Exp.syn_perform(Contexts.empty, a, edit_state)) {
  | Failed => raise(FailedAction)
  | CursorEscaped(_) => raise(CursorEscaped)
  | Succeeded(new_edit_state) =>
    let (ze, ty, u_gen) = new_edit_state;
    let new_edit_state =
      if (UHExp.is_complete(ZExp.erase(ze), false)) {
        (ze, ty, MetaVarGen.init);
      } else {
        (ze, ty, u_gen);
      };
    program |> put_edit_state(new_edit_state);
  };
};

exception HoleNotFound;
let move_to_hole = (u, program) => {
  let (ze, _, _) = program.edit_state;
  let holes = CursorPath_Exp.holes(ZExp.erase(ze), [], []);
  switch (CursorPath_common.steps_to_hole(holes, u)) {
  | None => raise(HoleNotFound)
  | Some(hole_steps) =>
    let e = ZExp.erase(ze);
    switch (CursorPath_Exp.of_steps(hole_steps, e)) {
    | None => raise(HoleNotFound)
    | Some(hole_path) => program |> perform_edit_action(MoveTo(hole_path))
    };
  };
};

let move_to_case_branch =
    (steps_to_case, branch_index, program): (t, Action_common.t) => {
  let steps_to_branch = steps_to_case @ [1 + branch_index];
  let new_program =
    perform_edit_action(
      MoveTo((steps_to_branch, OnDelim(1, After))),
      program,
    );
  (new_program, MoveTo((steps_to_branch, OnDelim(1, After))));
};

let get_doc = (~measure_program_get_doc: bool, ~memoize_doc: bool, program) => {
  TimeUtil.measure_time("Program.get_doc", measure_program_get_doc, () => {
    Lazy.force(
      UHDoc_Exp.mk,
      ~memoize=memoize_doc,
      ~enforce_inline=false,
      get_uhexp(program),
    )
  });
};

module EqHash = {
  type t = Pretty.Doc.t(unit);
  let equal = (===);
  let hash = Hashtbl.hash;
};

module EqHashtbl = Hashtbl.Make(EqHash);

let count_doc = (~doc: Pretty.Doc.t('annot)): int => {
  let seen = EqHashtbl.create(0);
  let rec go = (doc: Pretty.Doc.t('annot)): unit =>
    if (EqHashtbl.mem(seen, Obj.magic(doc))) {
      ();
    } else {
      EqHashtbl.add(seen, Obj.magic(doc), ());
      switch (doc.doc) {
      | Text(string) => ()
      | Cat(d1, d2) =>
        go(d1);
        go(d2);
      | Linebreak(int) => ()
      | Align(d) => go(d)
      | Annot(_, d) => go(d)
      | Fail(_) => ()
      | Choice(d1, d2) =>
        go(d1);
        go(d2);
      };
    };
  go(doc);
  EqHashtbl.length(seen);
};

let get_layout =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      program: t,
    )
    : Pretty.Layout.t(UHAnnot.t) => {
  let width = program.width;
  Hashtbl.clear(UHDoc_common.memoize_misses);
  let doc = get_doc(~measure_program_get_doc, ~memoize_doc, program);
  Printf.printf("doc size: %d\n", count_doc(doc));
  Printf.printf("misses:\n");
  Hashtbl.iter(
    (k, v) => {Printf.printf("  %4d: %5d\n", k, v)},
    UHDoc_common.memoize_misses,
  );
  let new_doc = Pretty.LayoutOfDoc.doc_new_of_old(doc);
  let new_layout =
    Pretty.LayoutOfDoc.new_layout_of_doc(~width, ~pos=0, new_doc);

  TimeUtil.measure_time(
    "LayoutOfDoc.layout_of_doc", measure_layoutOfDoc_layout_of_doc, () =>
    Pretty.LayoutOfDoc.layout_of_doc(~width, ~pos=0, doc)
  )
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
let decorate_var_uses = (ci: CursorInfo_common.t, l: UHLayout.t): UHLayout.t =>
  switch (ci.uses) {
  | None => l
  | Some(uses) =>
    uses
    |> List.fold_left(
         (l: UHLayout.t, use) =>
           l
           |> UHLayout.find_and_decorate_var_use(~steps=use)
           |> OptUtil.get(() => {
                failwith(
                  __LOC__
                  ++ ": could not find var use"
                  ++ Sexplib.Sexp.to_string(
                       CursorPath_common.sexp_of_steps(use),
                     ),
                )
              }),
         l,
       )
  };

let get_decorated_layout =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      program,
    ) => {
  let (steps, _) as path = program |> get_path;
  let ci = program |> get_cursor_info;
  program
  |> get_layout(
       ~measure_program_get_doc,
       ~measure_layoutOfDoc_layout_of_doc,
       ~memoize_doc,
     )
  |> decorate_caret(path)
  |> decorate_cursor(steps)
  |> decorate_var_uses(ci);
};

let get_cursor_map_z =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      program,
    ) => {
  let path = program |> get_path;
  // TODO figure out how to consolidate decoration
  program
  |> get_layout(
       ~measure_program_get_doc,
       ~measure_layoutOfDoc_layout_of_doc,
       ~memoize_doc,
     )
  |> decorate_caret(path)
  |> CursorMap.mk
  |> (
    fun
    | (_, None) => failwith(__LOC__ ++ ": no cursor found")
    | (map, Some(z)) => (map, z)
  );
};

let get_cursor_map =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      program,
    ) =>
  program
  |> get_cursor_map_z(
       ~measure_program_get_doc,
       ~measure_layoutOfDoc_layout_of_doc,
       ~memoize_doc,
     )
  |> fst;

let move_via_click =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      row_col,
      program,
    )
    : (t, Action_common.t) => {
  let (_, rev_path) =
    program
    |> get_cursor_map(
         ~measure_program_get_doc,
         ~measure_layoutOfDoc_layout_of_doc,
         ~memoize_doc,
       )
    |> CursorMap.find_nearest_within_row(row_col);
  let path = CursorPath_common.rev(rev_path);
  let new_program =
    program |> focus |> clear_start_col |> perform_edit_action(MoveTo(path));
  (new_program, MoveTo(path));
};

let move_via_key =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      move_key: MoveKey.t,
      program,
    )
    : (t, Action_common.t) => {
  let (cmap, ((row, col), _) as z) =
    program
    |> get_cursor_map_z(
         ~measure_program_get_doc,
         ~measure_layoutOfDoc_layout_of_doc,
         ~memoize_doc,
       );
  let (from_col, put_col_on_start) =
    switch (program.start_col_of_vertical_movement) {
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
    let path = CursorPath_common.rev(rev_path);
    let new_program =
      program |> update_start_col |> perform_edit_action(MoveTo(path));
    (new_program, MoveTo(path));
  };
};

let cursor_on_exp_hole = program =>
  program |> get_zexp |> ZExp.cursor_on_EmptyHole;
