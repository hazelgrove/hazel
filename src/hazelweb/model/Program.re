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
  [@deriving sexp]
  type term =
    | Focused(ZExp.t)
    | Unfocused(UHExp.t);

  [@deriving sexp]
  type t = {
    term,
    ty: HTyp.t,
    u_gen: MetaVarGen.t,
  };

  let get_focused_term = edit_state =>
    switch (edit_state.term) {
    | Unfocused(e) => ZExp.place_before(e)
    | Focused(ze) => ze
    };

  let focus = edit_state => {
    ...edit_state,
    term: Focused(get_focused_term(edit_state)),
  };
  let blur = edit_state =>
    switch (edit_state.term) {
    | Unfocused(_) => edit_state
    | Focused(ze) => {...edit_state, term: Unfocused(ZExp.erase(ze))}
    };
};

[@deriving sexp]
type current_splice = option((MetaVar.t, SpliceName.t));

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

let erase = Memo.general(~cache_size_bound=1000, ZExp.erase);
let get_uhexp = program =>
  switch (program.edit_state.term) {
  | Unfocused(e) => e
  | Focused(ze) => erase(ze)
  };

let get_zexp = program =>
  switch (program.edit_state.term) {
  | Unfocused(_) => None
  | Focused(ze) => Some(ze)
  };

let get_path = program => {
  let+ ze = get_zexp(program);
  CursorPath_Exp.of_z(ze);
};

exception MissingCursorInfo;
let cursor_info =
  Memo.general(
    ~cache_size_bound=1000,
    CursorInfo_Exp.syn_cursor_info((
      BuiltinFunctions.ctx,
      Livelits.initial_livelit_ctx,
    )),
  );
let get_cursor_info = (program: t) => {
  switch (program.edit_state.term) {
  | Unfocused(_) => None
  | Focused(ze) =>
    Some(cursor_info(ze) |> OptUtil.get(() => raise(MissingCursorInfo)))
  };
};

let get_decoration_paths = (program: t): UHDecorationPaths.t => {
  let current_term = get_path(program);
  let holes = CursorPath_Exp.holes(get_uhexp(program), [], []);
  let err_holes =
    holes
    |> List.filter_map((CursorPath.{steps, sort}) =>
         switch (sort) {
         | PatHole(_, TypeErr)
         | ExpHole(_, TypeErr) => Some(steps)
         | _ => None
         }
       );
  let var_err_holes =
    holes
    |> List.filter_map((CursorPath.{steps, sort}) =>
         switch (sort) {
         | PatHole(_, VarErr)
         | ExpHole(_, VarErr)
         | LivelitHole(_) => Some(steps)
         | _ => None
         }
       );
  let livelits =
    holes
    |> List.filter_map((CursorPath.{steps, sort}) =>
         switch (sort) {
         | ApLivelit(_) => Some(steps)
         | _ => None
         }
       );
  let var_uses =
    switch (get_cursor_info(program)) {
    | Some({uses: Some(uses), _}) => uses
    | _ => []
    };
  {current_term, err_holes, var_uses, var_err_holes, livelits};
};

module Elaborator_Exp = Elaborator_Exp.M(Statics_Exp);

exception DoesNotElaborate;
let expand = (~livelit_holes=false) =>
  Memo.general(
    ~cache_size_bound=1000,
    Elaborator_Exp.syn_elab(
      ~livelit_holes,
      (BuiltinFunctions.ctx, Livelits.initial_livelit_ctx),
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
  Memo.general(~cache_size_bound=1000, Eval.evaluate(~eval_livelit_holes));
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
  | BoxedValue(d) => ret(d, d' => Eval.BoxedValue(d'))
  | Indet(d) => ret(d, d' => Eval.Indet(d'))
  };
};

let get_doc = (~settings: Settings.t, program) => {
  let e = get_uhexp(program);
  let llview_ctx = Statics_Exp.build_ll_view_ctx(e);

  let doc =
    TimeUtil.measure_time(
      "Program.get_doc",
      settings.performance.measure && settings.performance.program_get_doc,
      () =>
      Lazy.force(
        UHDoc_Exp.mk,
        ~memoize=settings.memoize_doc,
        ~enforce_inline=false,
        (llview_ctx, e),
      )
    );
  let splice_docs = UHDoc_Exp.mk_splices(llview_ctx, e);
  (doc, splice_docs);
};

let get_layout = (~settings: Settings.t, program) => {
  let width = program.width;
  let (doc, splice_docs) = get_doc(~settings, program);
  let layout =
    TimeUtil.measure_time(
      "LayoutOfDoc.layout_of_doc",
      settings.performance.measure
      && settings.performance.layoutOfDoc_layout_of_doc,
      () =>
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
    (~settings: Settings.t, program): UHMeasuredLayout.with_splices => {
  let (l, splice_ls) = get_layout(~settings, program);
  let m = UHMeasuredLayout.mk(l);
  let splice_ms =
    splice_ls |> SpliceMap.map(SpliceMap.ApMap.map(UHMeasuredLayout.mk));
  (m, splice_ms);
};

let get_caret_position =
    (~settings: Settings.t, program)
    : option((MeasuredPosition.t, current_splice)) => {
  let m_with_splices = get_measured_layout(~settings, program);
  let+ path = get_path(program);
  UHMeasuredLayout.caret_position_of_path(path, m_with_splices)
  |> OptUtil.get(() => failwith("could not find caret"));
};

exception FailedAction;
exception CursorEscaped;
let perform_action =
    (~settings, ~move_via: option(MoveInput.t)=?, a: Action.t, program: t) => {
  let performed =
    switch (move_via, program.edit_state.term) {
    | (None | Some(Key(_)), Unfocused(_)) => program
    | _ =>
      let ze = EditState.get_focused_term(program.edit_state);
      let EditState.{ty, u_gen, _} = program.edit_state;
      let init_ctx = (BuiltinFunctions.ctx, Livelits.initial_livelit_ctx);
      switch (Action_Exp.syn_perform(init_ctx, a, (ze, ty, u_gen))) {
      | Failed => raise(FailedAction)
      | CursorEscaped(_) => raise(CursorEscaped)
      | Succeeded((ze, ty, u_gen)) =>
        let u_gen =
          UHExp.is_complete(ZExp.erase(ze), false) ? MetaVarGen.init : u_gen;
        {
          ...program,
          edit_state: {
            term: Focused(ze),
            ty,
            u_gen,
          },
        };
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
      | (None, Some((caret_position, _))) =>
        put_start_col(caret_position.col)
      }
    };
  update_start_col(performed);
};

exception NodeNotFound;
let move_to_node = (kind, u, program) => {
  let e = get_uhexp(program);
  let holes = CursorPath_Exp.holes(e, [], []);
  switch (CursorPath_common.steps_to_hole(holes, kind, u)) {
  | None => raise(NodeNotFound)
  | Some(hole_steps) =>
    switch (CursorPath_Exp.of_steps(hole_steps, e)) {
    | None => raise(NodeNotFound)
    | Some(hole_path) => Action.MoveTo(hole_path)
    }
  };
};

let move_to_case_branch = (steps_to_case, branch_index): Action.t => {
  let steps_to_branch = steps_to_case @ [1 + branch_index];
  Action.MoveTo((steps_to_branch, OnDelim(1, After)));
};

let cursor_on_inst_ =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_on_inst);
let cursor_on_inst = program => {
  let* ze = get_zexp(program);
  cursor_on_inst_(ze);
};

let cursor_through_insts_ =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_through_insts);
let cursor_through_insts = program => {
  switch (get_zexp(program)) {
  | None => []
  | Some(ze) => cursor_through_insts_(ze)
  };
};

let target_path_of_click_input =
    (
      ~settings: Settings.t,
      current_splice,
      target: MeasuredPosition.t,
      program,
    )
    : CursorPath.t => {
  let (path_prefix, m) = {
    let (m, splice_ms) = get_measured_layout(~settings, program);
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
  /*
     let (rev_path_suffix, _) =
       m
       |> UHMeasuredLayout.nearest_path_within_row(target)
       |> OptUtil.get(() => failwith("row with no caret positions"));
     CursorPath.(append(path_prefix, rev(rev_path_suffix)));
   */
  switch (UHMeasuredLayout.nearest_path_within_row(target, m)) {
  | None => ([], OnDelim(0, Before))
  | Some((rev_path_suffix, _)) =>
    CursorPath.(append(path_prefix, rev(rev_path_suffix)))
  };
};

let target_path_of_key_input =
    (~settings: Settings.t, move_key: MoveKey.t, program)
    : option(CursorPath.t) => {
  let* (caret_position, current_splice) =
    get_caret_position(~settings, program);
  let (path_prefix, m) = {
    let (m, splice_ms) = get_measured_layout(~settings, program);
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
  let from_col =
    switch (program.start_col_of_vertical_movement) {
    | None => caret_position.col
    | Some(col) => col
    };
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
  CursorPath.(append(path_prefix, rev(rev_path)));
};
