module Result_ = Result;
open Core_kernel;
module Result = Result_;

type t = {
  width: int,
  edit_state: Statics.edit_state,
};

let mk = (~width: int, edit_state: Statics.edit_state): t => {
  width,
  edit_state,
};

let get_width = program => program.width;

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
        NodeInstanceInfo.empty,
        NodeInstanceInfo.empty,
        d,
      );
    (d_renumbered, hii, llii, BoxedValue(d_renumbered));
  | Indet(d) =>
    let (d_renumbered, hii, llii) =
      Dynamics.Exp.renumber(
        [],
        NodeInstanceInfo.empty,
        NodeInstanceInfo.empty,
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
  switch (ci.uses) {
  | None => l
  | Some(uses) =>
    uses
    |> List.fold(~init=l, ~f=(l: UHLayout.t, use) =>
         l
         |> UHLayout.find_and_decorate_var_use(~steps=use)
         |> OptUtil.get(() => {
              failwith(
                __LOC__
                ++ ": could not find var use"
                ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(use)),
              )
            })
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

let perform_move_action = (move_key, program) => {
  let (cmap, z) = program |> get_cursor_map_z;
  switch (cmap |> CursorMap.move(move_key, z)) {
  | None => raise(CursorEscaped)
  | Some((_, rev_path)) =>
    let path = CursorPath.rev(rev_path);
    program |> perform_edit_action(MoveTo(path));
  };
};
