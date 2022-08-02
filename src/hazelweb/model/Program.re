open Sexplib.Std;

module UHDoc_Exp = UHDoc_Exp.Make(WeakMemo);
module Memo = Core_kernel.Memo;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

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
    CursorInfo_Exp.syn_cursor_info(InitialContext.ctx),
  );
let get_cursor_info = (program: t) =>
  program
  |> get_zexp
  |> cursor_info
  |> OptUtil.get(() => raise(MissingCursorInfo));

let get_decoration_paths = (program: t): UHDecorationPaths.t => {
  let current_term = program.is_focused ? Some(get_path(program)) : None;
  let (err_holes, var_err_holes) =
    CursorPath_Exp.holes(get_uhexp(program), [], [])
    |> List.filter_map(hole_info =>
         switch (CursorPath.get_sort(hole_info)) {
         | TPatHole(Empty)
         | TypHole => None
         | TPatHole(_)
         | TyVarHole =>
           Some((CursorPath.VarErr, CursorPath.get_steps(hole_info)))
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
    switch (get_cursor_info(program)) {
    | {uses: Some(uses), _} => uses
    | _ => []
    };
  let tyvar_uses =
    switch (get_cursor_info(program)) {
    | {tyuses: Some(tyuses), _} => tyuses
    | _ => []
    };
  {current_term, err_holes, var_uses, tyvar_uses, var_err_holes};
};

let rec renumber_result_only =
        (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
        : (DHExp.t, HoleInstanceInfo.t) =>
  switch (d) {
  | BoundVar(_)
  | InvalidText(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (d, hii)
  | Let(dp, d1, d2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    (Let(dp, d1, d2), hii);
  | TyAlias(dp, dty, d3) =>
    let (d3, hii) = renumber_result_only(path, hii, d3);
    (TyAlias(dp, dty, d3), hii);
  | FixF(x, ty, d1) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (FixF(x, ty, d1), hii);
  | Fun(x, ty, d1) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (Fun(x, ty, d1), hii);
  | Ap(d1, d2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    (Ap(d1, d2), hii);
  | ApBuiltin(ident, args) =>
    let (hii, args) =
      List.fold_right(
        (d1, (hii, acc)) => {
          let (d1, hii) = renumber_result_only(path, hii, d1);
          (hii, [d1, ...acc]);
        },
        List.rev(args),
        (hii, []),
      );
    (ApBuiltin(ident, args), hii);
  | TypFun(tp, d1) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (TypFun(tp, d1), hii);
  | TypApp(d1, dty) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (TypApp(d1, dty), hii);
  | BinBoolOp(op, d1, d2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    (BinBoolOp(op, d1, d2), hii);
  | BinIntOp(op, d1, d2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    (BinIntOp(op, d1, d2), hii);
  | BinFloatOp(op, d1, d2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    (BinFloatOp(op, d1, d2), hii);
  | Inj(ty, side, d1) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (Inj(ty, side, d1), hii);
  | Pair(d1, d2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    (Pair(d1, d2), hii);
  | Cons(d1, d2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    (Cons(d1, d2), hii);
  | ConsistentCase(Case(d1, rules, n)) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (drules, hii) = renumber_result_only_rules(path, hii, rules);
    (ConsistentCase(Case(d1, drules, n)), hii);
  | InconsistentBranches(u, _, sigma, Case(d1, rules, n)) =>
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (drules, hii) = renumber_result_only_rules(path, hii, rules);
    (InconsistentBranches(u, i, sigma, Case(d1, drules, n)), hii);
  | EmptyHole(u, _, sigma) =>
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (EmptyHole(u, i, sigma), hii);
  | NonEmptyHole(reason, u, _, sigma, d1) =>
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (NonEmptyHole(reason, u, i, sigma, d1), hii);
  | FreeVar(u, _, sigma, x) =>
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (FreeVar(u, i, sigma, x), hii);
  | Keyword(u, _, sigma, k) =>
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (Keyword(u, i, sigma, k), hii);
  | Cast(d1, ty1, ty2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (Cast(d1, ty1, ty2), hii);
  | FailedCast(d1, ty1, ty2) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (FailedCast(d1, ty1, ty2), hii);
  | InvalidOperation(d, err) =>
    let (d, hii) = renumber_result_only(path, hii, d);
    (InvalidOperation(d, err), hii);
  }
and renumber_result_only_rules =
    (path: InstancePath.t, hii: HoleInstanceInfo.t, rules: list(DHExp.rule))
    : (list(DHExp.rule), HoleInstanceInfo.t) =>
  rules
  |> List.fold_left(
       (b, r: DHExp.rule) => {
         let (rs, hii) = b;
         switch (r) {
         | Rule(dp, d) =>
           let (dp, hii) =
             Elaborator_Pat.renumber_result_only(path, hii, dp);
           let (d, hii) = renumber_result_only(path, hii, d);
           (rs @ [DHExp.Rule(dp, d)], hii);
         };
       },
       ([], hii),
     );

let rec renumber_sigmas_only =
        (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
        : (DHExp.t, HoleInstanceInfo.t) =>
  switch (d) {
  | BoundVar(_)
  | InvalidText(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (d, hii)
  | Let(dp, d1, d2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    (Let(dp, d1, d2), hii);
  | TyAlias(dp, dty, d3) =>
    let (d3, hii) = renumber_sigmas_only(path, hii, d3);
    (TyAlias(dp, dty, d3), hii);
  | FixF(x, ty, d1) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    (FixF(x, ty, d1), hii);
  | Fun(x, ty, d1) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    (Fun(x, ty, d1), hii);
  | Ap(d1, d2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    (Ap(d1, d2), hii);
  | ApBuiltin(ident, args) =>
    let (hii, args) =
      List.fold_right(
        (d1, (hii, acc)) => {
          let (d1, hii) = renumber_sigmas_only(path, hii, d1);
          (hii, [d1, ...acc]);
        },
        List.rev(args),
        (hii, []),
      );
    (ApBuiltin(ident, args), hii);
  | TypFun(tp, d1) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (TypFun(tp, d1), hii);
  | TypApp(d1, dty) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    (TypApp(d1, dty), hii);
  | BinBoolOp(op, d1, d2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    (BinBoolOp(op, d1, d2), hii);
  | BinIntOp(op, d1, d2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    (BinIntOp(op, d1, d2), hii);
  | BinFloatOp(op, d1, d2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    (BinFloatOp(op, d1, d2), hii);
  | Inj(ty, side, d1) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    (Inj(ty, side, d1), hii);
  | Pair(d1, d2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    (Pair(d1, d2), hii);
  | Cons(d1, d2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    (Cons(d1, d2), hii);
  | ConsistentCase(Case(d1, rules, n)) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (rules, hii) = renumber_sigmas_only_rules(path, hii, rules);
    (ConsistentCase(Case(d1, rules, n)), hii);
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
    let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
    let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (rules, hii) = renumber_sigmas_only_rules(path, hii, rules);
    (InconsistentBranches(u, i, sigma, Case(d1, rules, n)), hii);
  | EmptyHole(u, i, sigma) =>
    let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
    let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
    (EmptyHole(u, i, sigma), hii);
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
    let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    (NonEmptyHole(reason, u, i, sigma, d1), hii);
  | FreeVar(u, i, sigma, x) =>
    let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
    let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
    (FreeVar(u, i, sigma, x), hii);
  | Keyword(u, i, sigma, k) =>
    let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
    let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
    (Keyword(u, i, sigma, k), hii);
  | Cast(d1, ty1, ty2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    (Cast(d1, ty1, ty2), hii);
  | FailedCast(d1, ty1, ty2) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    (FailedCast(d1, ty1, ty2), hii);
  | InvalidOperation(d, err) =>
    let (d, hii) = renumber_sigmas_only(path, hii, d);
    (InvalidOperation(d, err), hii);
  }
and renumber_sigmas_only_rules =
    (path: InstancePath.t, hii: HoleInstanceInfo.t, rules: list(DHExp.rule))
    : (list(DHExp.rule), HoleInstanceInfo.t) =>
  rules
  |> List.fold_left(
       (b, r: DHExp.rule) => {
         let (rs, hii) = b;
         switch (r) {
         | Rule(dp, d) =>
           /* pattern holes don't have environments */
           let (d, hii) = renumber_sigmas_only(path, hii, d);
           (rs @ [DHExp.Rule(dp, d)], hii);
         };
       },
       ([], hii),
     )
and renumber_sigma =
    (
      path: InstancePath.t,
      u: MetaVar.t,
      i: MetaVarInst.t,
      hii: HoleInstanceInfo.t,
      sigma: Environment.t,
    )
    : (Environment.t, HoleInstanceInfo.t) => {
  let (sigma, hii) =
    List.fold_right(
      (xd: (Var.t, DHExp.t), acc: (Environment.t, HoleInstanceInfo.t)) => {
        let (x, d) = xd;
        let (sigma_in, hii) = acc;
        let path = [((u, i), x), ...path];
        let (d, hii) = renumber_result_only(path, hii, d);
        let sigma_out = [(x, d), ...sigma_in];
        (sigma_out, hii);
      },
      sigma,
      ([], hii),
    );

  List.fold_right(
    (xd: (Var.t, DHExp.t), acc: (Environment.t, HoleInstanceInfo.t)) => {
      let (x, d) = xd;
      let (sigma_in, hii) = acc;
      let path = [((u, i), x), ...path];
      let (d, hii) = renumber_sigmas_only(path, hii, d);
      let sigma_out = [(x, d), ...sigma_in];
      (sigma_out, hii);
    },
    sigma,
    ([], hii),
  );
};

let renumber =
    (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
    : (DHExp.t, HoleInstanceInfo.t) => {
  let (d, hii) = renumber_result_only(path, hii, d);
  renumber_sigmas_only(path, hii, d);
};

exception DoesNotElaborate;
let elaborate =
  Memo.general(
    ~cache_size_bound=1000,
    Elaborator_Exp.elab(InitialContext.ctx, Delta.empty),
  );
let get_elaboration = (program: t): DHExp.t =>
  switch (program |> get_uhexp |> elaborate) {
  | DoesNotElaborate => raise(DoesNotElaborate)
  | Elaborates(d, _, _) => d
  };

exception EvalError(EvaluatorError.t);
let evaluate = Memo.general(~cache_size_bound=1000, Evaluator.evaluate);
let get_result = (program: t): Result.t =>
  switch (program |> get_elaboration |> evaluate) {
  | BoxedValue(d) =>
    let (d_renumbered, hii) = renumber([], HoleInstanceInfo.empty, d);
    (d_renumbered, hii, BoxedValue(d_renumbered));
  | Indet(d) =>
    let (d_renumbered, hii) = renumber([], HoleInstanceInfo.empty, d);
    (d_renumbered, hii, Indet(d_renumbered));
  | exception (EvaluatorError.Exception(reason)) => raise(EvalError(reason))
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

let get_caret_position = (~settings: Settings.t, program): MeasuredPosition.t => {
  let m = get_measured_layout(~settings, program);
  let path = get_path(program);
  UHMeasuredLayout.caret_position_of_path(path, m)
  |> OptUtil.get(() => failwith("could not find caret"));
};

exception FailedAction;
exception CursorEscaped;
let perform_edit_action = (a, program) => {
  let edit_state = program.edit_state;
  print_endline("perform_edit_action");
  switch (Action_Exp.syn_perform(InitialContext.ctx, a, edit_state)) {
  | Failed => raise(FailedAction)
  | CursorEscaped(_) => raise(CursorEscaped)
  | Succeeded(new_edit_state) =>
    print_endline("perform_edit_action: succeeded");
    let (ze, ty, id_gen) = new_edit_state;
    let new_edit_state =
      if (UHExp.is_complete(ZExp.erase(ze))) {
        (ze, ty, IDGen.init);
      } else {
        (ze, ty, id_gen);
      };
    print_endline("perform_edit_action: finished");
    program |> put_edit_state(new_edit_state);
  };
  // };
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
    | Some(hole_path) => Action.MoveTo(hole_path)
    };
  };
};

let move_to_case_branch = (steps_to_case, branch_index): Action.t => {
  let steps_to_branch = steps_to_case @ [1 + branch_index];
  Action.MoveTo((steps_to_branch, OnDelim(1, After)));
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
    program |> focus |> clear_start_col |> perform_edit_action(MoveTo(path));
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

let cursor_on_exp_hole_ =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_on_EmptyHole);
let cursor_on_exp_hole = program => program |> get_zexp |> cursor_on_exp_hole_;
