open Sexplib.Std;

module Memo = Core_kernel.Memo;

[@deriving sexp]
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

let erase = Memo.general(~cache_size_bound=1000, ZExp.erase);
let get_uhexp = program => program |> get_zexp |> erase;

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
let cursor_info =
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
  |> cursor_info
  |> OptUtil.get(() => raise(MissingCursorInfo));
};

exception DoesNotExpand;
let expand = (~livelit_holes=false) =>
  Memo.general(
    ~cache_size_bound=1000,
    Dynamics.Exp.syn_expand(
      ~livelit_holes,
      (VarCtx.empty, Livelits.initial_livelit_ctx),
      Delta.empty,
    ),
  );
let get_expansion = (~livelit_holes=false, program: t): DHExp.t =>
  switch (program |> get_uhexp |> expand(~livelit_holes)) {
  | DoesNotExpand => raise(DoesNotExpand)
  | Expands(d, _, _) => d
  };

exception InvalidInput;
let evaluate = (~eval_livelit_holes=false) => {
  Memo.general(
    ~cache_size_bound=1000,
    Dynamics.Evaluator.evaluate(~eval_livelit_holes),
  );
};

let fill_and_resume_llii =
    (llii: LivelitInstanceInfo.t): LivelitInstanceInfo.t =>
  llii
  |> MetaVarMap.map(
       List.map(((env, path, si)) => {
         let env =
           env
           |> List.map(((v, d)) =>
                (
                  v,
                  switch (d) {
                  | DHExp.BoundVar(_) => d
                  | _ =>
                    switch (evaluate(~eval_livelit_holes=true, d)) {
                    | InvalidInput(msg) =>
                      failwith(
                        Printf.sprintf(
                          "Unexpected evaluation failure during env fill and resume: %d",
                          msg,
                        ),
                      )
                    | BoxedValue(d)
                    | Indet(d) => d
                    }
                  },
                )
              );
         let sim =
           SpliceInfo.splice_map(si)
           |> NatMap.map(((typ, d_opt)) =>
                (
                  typ,
                  d_opt
                  |> OptUtil.and_then(d =>
                       switch (evaluate(~eval_livelit_holes=true, d)) {
                       | InvalidInput(_) => None
                       | BoxedValue(d)
                       | Indet(d) => Some(d)
                       }
                     ),
                )
              );
         let si = SpliceInfo.update_splice_map(si, sim);
         (env, path, si);
       }),
     );

let get_result = (program: t): Result.t => {
  let renumber =
    Dynamics.Exp.renumber(
      [],
      HoleInstanceInfo.empty,
      LivelitInstanceInfo.empty,
    );
  let ret = (d, wrapper) =>
    switch (program |> get_expansion(~livelit_holes=true) |> evaluate) {
    | InvalidInput(_) => failwith("Failed evaluation only with livelit_holes")
    | BoxedValue(d')
    | Indet(d') =>
      let (d_renumbered, hii, _) = renumber(d);
      let (_, _, llii) = renumber(d');
      let llii = fill_and_resume_llii(llii);
      (d_renumbered, hii, llii, wrapper(d_renumbered));
    };
  switch (program |> get_expansion(~livelit_holes=false) |> evaluate) {
  | InvalidInput(_) => raise(InvalidInput)
  | BoxedValue(d) => ret(d, d' => Dynamics.Evaluator.BoxedValue(d'))
  | Indet(d) => ret(d, d' => Dynamics.Evaluator.Indet(d'))
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

exception NodeNotFound;
let move_to_node = (kind, u, program) => {
  let (ze, _, _) = program |> get_edit_state;
  let holes = CursorPath.Exp.holes(ZExp.erase(ze), [], []);
  switch (CursorPath.steps_to_hole(holes, kind, u)) {
  | None => raise(NodeNotFound)
  | Some(hole_steps) =>
    let e = ZExp.erase(ze);
    switch (CursorPath.Exp.of_steps(hole_steps, e)) {
    | None => raise(NodeNotFound)
    | Some(hole_path) => program |> perform_edit_action(MoveTo(hole_path))
    };
  };
};

let move_to_case_branch =
    (steps_to_case, branch_index, program): (t, Action.t) => {
  let steps_to_branch = steps_to_case @ [1 + branch_index];
  let new_program =
    perform_edit_action(
      MoveTo((steps_to_branch, OnDelim(1, After))),
      program,
    );
  (new_program, MoveTo((steps_to_branch, OnDelim(1, After))));
};

let get_doc = (~measure_program_get_doc: bool, ~memoize_doc: bool, program) => {
  let e = get_uhexp(program);
  let doc =
    TimeUtil.measure_time("Program.get_doc", measure_program_get_doc, () =>
      Lazy.force(UHDoc.Exp.mk, ~memoize=memoize_doc, ~enforce_inline=false, e)
    );
  let splice_docs = UHDoc.Exp.mk_splices(e);
  (doc, splice_docs);
};

let cursor_on_inst_ =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_on_inst);
let cursor_on_inst = program => program |> get_zexp |> cursor_on_inst_;

let cursor_through_insts_ =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_through_insts);
let cursor_through_insts = program =>
  program |> get_zexp |> cursor_through_insts_;

let get_layout =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      program,
    ) => {
  let width = program |> get_width;
  let (doc, splice_docs) =
    program |> get_doc(~measure_program_get_doc, ~memoize_doc);
  let layout =
    TimeUtil.measure_time(
      "LayoutOfDoc.layout_of_doc", measure_layoutOfDoc_layout_of_doc, () =>
      Pretty.LayoutOfDoc.layout_of_doc(~width, ~pos=0, doc)
    )
    |> OptUtil.get(() => failwith("unimplemented: layout failure"));
  let rec splice_layouts = l =>
    l
    |> UHLayout.fold(
         ~linebreak=MetaVarMap.empty,
         ~text=_ => MetaVarMap.empty,
         ~align=ls => ls,
         ~cat=(@),
         ~annot=
           (pos, annot, ls) =>
             switch (annot) {
             | LivelitView({llu, _}) =>
               let ap_splice_layouts =
                 MetaVarMap.lookup(splice_docs, llu)
                 |> OptUtil.get(() =>
                      failwith(
                        "no doc for livelit ap " ++ string_of_int(llu),
                      )
                    )
                 |> SpliceMap.ApMap.map(doc => {
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
                    });
               SpliceMap.ApMap.fold(
                 (_, ap_splice_layout, ls) => {
                   let inner_splice_layouts =
                     splice_layouts(ap_splice_layout);
                   MetaVarMap.union(ls, inner_splice_layouts);
                 },
                 ap_splice_layouts,
                 MetaVarMap.extend_unique(ls, (llu, ap_splice_layouts)),
               );
             | _ => ls
             },
       );
  (layout, splice_layouts(layout));
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
      opt_splice,
      row_col,
      program,
    )
    : (t, Action.t) => {
  let cmap = {
    let (cmap, splice_cmaps) =
      program
      |> get_cursor_map(
           ~measure_program_get_doc,
           ~measure_layoutOfDoc_layout_of_doc,
           ~memoize_doc,
         );
    switch (opt_splice) {
    | None => cmap
    | Some((u, splice_name)) =>
      splice_cmaps |> SpliceMap.get_splice(u, splice_name)
    };
  };
  let (_, rev_path) = cmap |> CursorMap.find_nearest_within_row(row_col);
  let path = CursorPath.rev(rev_path);
  let new_program =
    program |> focus |> clear_start_col |> perform_edit_action(MoveTo(path));
  (new_program, MoveTo(path));
};

let move_via_key =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      move_key: JSUtil.MoveKey.t,
      program,
    )
    : (t, Action.t) => {
  let (cmap, z) = {
    let ((cmap, splice_cmaps), (opt_splice, z)) =
      program
      |> get_cursor_map_z(
           ~measure_program_get_doc,
           ~measure_layoutOfDoc_layout_of_doc,
           ~memoize_doc,
         );
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
    let new_program =
      program |> update_start_col |> perform_edit_action(MoveTo(path));
    (new_program, MoveTo(path));
  };
};
