[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Program.EditState.t,
};

let mk = (info: CardInfo.t): t => {
  let edit_state: Program.EditState.t = {
    let init_ctx = (VarCtx.empty, BuiltinLivelits.ctx);
    let (term, ty, u_gen) =
      Statics_Exp.fix_and_renumber_holes(init_ctx, info.init_term);
    {term, ty, u_gen, focus: None};
  };
  {info, edit_state};
};
