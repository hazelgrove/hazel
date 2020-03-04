module Result_ = Result;
open Core_kernel;
module Result = Result_;

type context_inspector = {
  prev_state: option(HoleInstance.t),
  next_state: option(HoleInstance.t),
};

type t = {
  edit_state: Statics.edit_state,
  selected_instance: option(HoleInstance.t),
  context_inspector,
  user_selected_instances: UserSelectedInstances.t,
};

let mk = (edit_state: Statics.edit_state): t => {
  edit_state,
  selected_instance: None,
  context_inspector: {
    prev_state: None,
    next_state: None,
  },
  user_selected_instances: UserSelectedInstances.init,
};

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

let _cursor_info =
  Memo.general(
    ~cache_size_bound=1000,
    CursorInfo.Exp.syn_cursor_info(Contexts.empty),
  );

exception MissingCursorInfo;
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

let _expand =
  Memo.general(
    ~cache_size_bound=1000,
    Dynamics.Exp.syn_expand(Contexts.empty, Delta.empty),
  );
let _evaluate =
  Memo.general(~cache_size_bound=1000, Dynamics.Evaluator.evaluate);

// TODO memoize
exception InvalidInput;
exception DoesNotExpand;
let get_result = (program: t): Result.t => {
  open Dynamics;
  let e = program |> get_uhexp;
  switch (_expand(e)) {
  | DoesNotExpand => raise(DoesNotExpand)
  | Expands(d, _, _) =>
    switch (_evaluate(d)) {
    | InvalidInput(_) => raise(InvalidInput)
    | BoxedValue(d) =>
      let (d_renumbered, hii) = Exp.renumber([], HoleInstanceInfo.empty, d);
      (d_renumbered, hii, BoxedValue(d_renumbered));
    | Indet(d) =>
      let (d_renumbered, hii) = Exp.renumber([], HoleInstanceInfo.empty, d);
      (d_renumbered, hii, Indet(d_renumbered));
    }
  };
};

let get_selected_instance = program => program.selected_instance;
let put_selected_instance = ((u, i) as inst, program) => {
  let (_, hii, _) = program |> get_result;
  {
    ...program,
    selected_instance: Some(inst),
    user_selected_instances:
      program.user_selected_instances |> UserSelectedInstances.update(inst),
    context_inspector: {
      prev_state: i > 0 ? Some((u, i - 1)) : None,
      next_state:
        i < HoleInstanceInfo.num_instances(hii, u) - 1
          ? Some((u, i + 1)) : None,
    },
  };
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
  Memo.general(
    ~cache_size_bound=1000,
    TermDoc.Exp.mk(~steps=[], ~enforce_inline=false),
  );
let get_doc = program => {
  let e = program |> get_uhexp;
  _doc(e);
};
