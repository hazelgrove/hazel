module Result_ = Result;
open Core_kernel;
module Result = Result_;

type t = {edit_state: Statics.edit_state};

let mk = (edit_state: Statics.edit_state): t => {edit_state: edit_state};

let get_edit_state = program => program.edit_state;
let put_edit_state = (edit_state, _program) => {edit_state: edit_state};

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
  let ze = program |> get_zexp;
  switch (_cursor_info(ze)) {
  | None => raise(MissingCursorInfo)
  | Some(ci) =>
    /* uncomment to see where variable is used
           switch (ci.node) {
           | Pat(VarPat(_, uses)) =>
             JSUtil.log_sexp(UsageAnalysis.sexp_of_uses_list(uses))
           | _ => JSUtil.log("not varpat")
           };
       */
    ci
  };
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
let _evaluate =
  Memo.general(~cache_size_bound=1000, Dynamics.Evaluator.evaluate);
let get_result = (program: t): Result.t =>
  switch (program |> get_expansion |> _evaluate) {
  | InvalidInput(_) => raise(InvalidInput)
  | BoxedValue(d) =>
    let (d_renumbered, hii) =
      Dynamics.Exp.renumber([], NodeInstanceInfo.empty, d);
    (d_renumbered, hii, BoxedValue(d_renumbered));
  | Indet(d) =>
    let (d_renumbered, hii) =
      Dynamics.Exp.renumber([], NodeInstanceInfo.empty, d);
    (d_renumbered, hii, Indet(d_renumbered));
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
  Memo.general(
    ~cache_size_bound=1000,
    UHDoc.Exp.mk(
      ~steps=[],
      ~enforce_inline=false,
      ~ctx=Livelits.initial_livelit_view_ctx,
    ),
  );
let get_doc = program => {
  let e = program |> get_uhexp;
  _doc(e);
};

let _cursor_on_exp_hole =
  Memo.general(~cache_size_bound=1000, ZExp.cursor_on_EmptyHole);
let cursor_on_exp_hole = program => program |> get_zexp |> _cursor_on_exp_hole;
