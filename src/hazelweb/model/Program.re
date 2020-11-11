open Sexplib.Std;

module Memo = Core_kernel.Memo;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

[@deriving sexp]
type current_splice = option((MetaVar.t, SpliceName.t));

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

let erase = Memo.general(~cache_size_bound=1000, ZExp.erase);
let get_uhexp = program => program |> get_zexp |> erase;

let get_path = program => program |> get_zexp |> CursorPath_Exp.of_z;
let get_steps = program => {
  let (steps, _) = program |> get_path;
  steps;
};

exception MissingCursorInfo;
let cursor_info =
  Memo.general(
    ~cache_size_bound=1000,
    CursorInfo_Exp.syn_cursor_info((
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

let get_decoration_paths = (program: t): UHDecorationPaths.t => {
  let current_term = program.is_focused ? Some(get_path(program)) : None;
  let holes = CursorPath_Exp.holes(get_uhexp(program), [], []);
  let err_holes =
    holes
    |> List.filter_map((CursorPath_common.{steps, sort}) =>
         switch (sort) {
         | PatHole(_, TypeErr)
         | ExpHole(_, TypeErr) => Some(steps)
         | _ => None
         }
       );
  let var_err_holes =
    holes
    |> List.filter_map((CursorPath_common.{steps, sort}) =>
         switch (sort) {
         | PatHole(_, VarErr)
         | ExpHole(_, VarErr)
         | LivelitHole(_) => Some(steps)
         | _ => None
         }
       );
  let livelits =
    holes
    |> List.filter_map((CursorPath_common.{steps, sort}) =>
         switch (sort) {
         | ApLivelit(_) => Some(steps)
         | _ => None
         }
       );
  let var_uses =
    switch (get_cursor_info(program)) {
    | {uses: Some(uses), _} => uses
    | _ => []
    };
  {current_term, err_holes, var_uses, var_err_holes, livelits};
};

exception DoesNotElaborate;
let expand = (~livelit_holes=false) =>
  Memo.general(
    ~cache_size_bound=1000,
    Elaborator_Exp.syn_elab(
      ~livelit_holes,
      (VarCtx.empty, Livelits.initial_livelit_ctx),
      Delta.empty,
    ),
  );
let get_expansion = (~livelit_holes=false, program: t): DHExp.t =>
  switch (program |> get_uhexp |> expand(~livelit_holes)) {
  | DoesNotElaborate => raise(DoesNotElaborate)
  | Elaborates(d, _, _) => d
  };

exception InvalidInput;
let evaluate = (~eval_livelit_holes=false) => {
  Memo.general(
    ~cache_size_bound=1000,
    Evaluator.evaluate(~eval_livelit_holes),
  );
};

let fill_and_resume_llii =
    (llii: LivelitInstanceInfo.t): LivelitInstanceInfo.t =>
  llii
  |> MetaVarMap.map(
       List.map(((env, path, (si, dargs_opt))) => {
         let eval_ = (kind, d) =>
           switch (evaluate(~eval_livelit_holes=true, d)) {
           | InvalidInput(msg) =>
             failwith(
               Printf.sprintf(
                 "Unexpected evaluation failure during %s fill and resume: %d",
                 kind,
                 msg,
               ),
             )
           | BoxedValue(d)
           | Indet(d) => d
           };
         let env =
           env
           |> List.map(((v, d)) =>
                (
                  v,
                  switch (d) {
                  | DHExp.BoundVar(_) => d
                  | _ => eval_("env", d)
                  },
                )
              );
         let sim =
           SpliceInfo.splice_map(si)
           |> IntMap.map(((typ, d_opt)) =>
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
         let dargs_opt =
           dargs_opt
           |> List.map(((v, darg_opt)) =>
                (v, darg_opt |> Option.map(eval_("livelit args")))
              );
         (env, path, (si, dargs_opt));
       }),
     );

let get_result = (program: t): Result.t => {
  let renumber =
    Elaborator_Exp.renumber(
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
  | BoxedValue(d) => ret(d, d' => Evaluator.BoxedValue(d'))
  | Indet(d) => ret(d, d' => Evaluator.Indet(d'))
  };
};

exception FailedAction;
exception CursorEscaped;
let perform_edit_action = (a, program) => {
  let edit_state = program.edit_state;
  switch (
    Action_Exp.syn_perform(
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
  let (ze, _, _) = program.edit_state;
  let holes = CursorPath_Exp.holes(ZExp.erase(ze), [], []);
  switch (CursorPath_common.steps_to_hole(holes, kind, u)) {
  | None => raise(NodeNotFound)
  | Some(hole_steps) =>
    let e = ZExp.erase(ze);
    switch (CursorPath_Exp.of_steps(hole_steps, e)) {
    | None => raise(NodeNotFound)
    | Some(hole_path) => Action_common.MoveTo(hole_path)
    };
  };
};

let move_to_case_branch = (steps_to_case, branch_index): Action_common.t => {
  let steps_to_branch = steps_to_case @ [1 + branch_index];
  Action_common.MoveTo((steps_to_branch, OnDelim(1, After)));
};

let get_doc = (~measure_program_get_doc: bool, ~memoize_doc: bool, program) => {
  let e = get_uhexp(program);
  let doc =
    TimeUtil.measure_time("Program.get_doc", measure_program_get_doc, () =>
      Lazy.force(UHDoc_Exp.mk, ~memoize=memoize_doc, ~enforce_inline=false, e)
    );
  let splice_docs = UHDoc_Exp.mk_splices(e);
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
  let width = program.width;
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
         ~cat=MetaVarMap.union((_, _, _) => None),
         ~annot=
           (pos, annot, ls) =>
             switch (annot) {
             | LivelitView({llu, _}) =>
               let ap_splice_layouts =
                 MetaVarMap.find_opt(llu, splice_docs)
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
                   MetaVarMap.union(
                     (_, _, _) => None,
                     ls,
                     inner_splice_layouts,
                   );
                 },
                 ap_splice_layouts,
                 MetaVarMap.add(llu, ap_splice_layouts, ls),
               );
             | _ => ls
             },
       );
  (layout, splice_layouts(layout));
};
let get_measured_layout =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      program,
    )
    : UHMeasuredLayout.with_splices => {
  let (l, splice_ls) =
    program
    |> get_layout(
         ~measure_program_get_doc,
         ~measure_layoutOfDoc_layout_of_doc,
         ~memoize_doc,
       );
  let m = UHMeasuredLayout.mk(l);
  let splice_ms =
    splice_ls |> SpliceMap.map(SpliceMap.ApMap.map(UHMeasuredLayout.mk));
  (m, splice_ms);
};

let get_caret_position =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      program,
    )
    : (MeasuredPosition.t, current_splice) => {
  let m_with_splices =
    get_measured_layout(
      ~measure_program_get_doc,
      ~measure_layoutOfDoc_layout_of_doc,
      ~memoize_doc,
      program,
    );
  let path = get_path(program);
  UHMeasuredLayout.caret_position_of_path(path, m_with_splices)
  |> OptUtil.get(() => failwith("could not find caret"));
};

let move_via_click =
    (
      ~measure_program_get_doc: bool,
      ~measure_layoutOfDoc_layout_of_doc: bool,
      ~memoize_doc: bool,
      current_splice,
      target: MeasuredPosition.t,
      program,
    )
    : (t, Action_common.t) => {
  let (path_prefix, m) = {
    let (m, splice_ms) =
      get_measured_layout(
        ~measure_program_get_doc,
        ~measure_layoutOfDoc_layout_of_doc,
        ~memoize_doc,
        program,
      );
    switch (current_splice) {
    | None => ([], m)
    | Some((u, splice_name)) =>
      let holes = CursorPath_Exp.holes(get_uhexp(program), [], []);
      let steps =
        CursorPath_common.steps_to_hole(holes, Livelit, u)
        |> OptUtil.get(() => raise(NodeNotFound));
      (
        steps @ [splice_name],
        SpliceMap.get_splice(u, splice_name, splice_ms),
      );
    };
  };
  let path_suffix =
    UHMeasuredLayout.nearest_path_within_row(target, m)
    |> OptUtil.get(() => failwith("row with no caret positions"))
    |> fst
    |> CursorPath_common.rev;
  let path = CursorPath_common.append(path_prefix, path_suffix);
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
  let (caret_position, current_splice) =
    get_caret_position(
      ~measure_program_get_doc,
      ~measure_layoutOfDoc_layout_of_doc,
      ~memoize_doc,
      program,
    );
  let (path_prefix, m) = {
    let (m, splice_ms) =
      get_measured_layout(
        ~measure_program_get_doc,
        ~measure_layoutOfDoc_layout_of_doc,
        ~memoize_doc,
        program,
      );
    switch (current_splice) {
    | None => ([], m)
    | Some((u, splice_name)) =>
      let steps =
        CursorPath_common.steps_to_hole(
          CursorPath_Exp.holes(get_uhexp(program), [], []),
          Livelit,
          u,
        )
        |> OptUtil.get(() => raise(NodeNotFound));
      (
        steps @ [splice_name],
        SpliceMap.get_splice(u, splice_name, splice_ms),
      );
    };
  };
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
    let path = CursorPath_common.(append(path_prefix, rev(rev_path)));
    let new_program =
      program |> update_start_col |> perform_edit_action(MoveTo(path));
    (new_program, MoveTo(path));
  };
};
