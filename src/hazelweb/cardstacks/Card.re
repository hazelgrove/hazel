[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Statics_common.edit_state,
};

let mk = (info: CardInfo.t): t => {
  info,
  edit_state:
    Statics_Exp.fix_and_renumber_holes_z(
      (VarCtx.empty, Livelits.initial_livelit_ctx),
      info.init_zexp,
    ),
};
