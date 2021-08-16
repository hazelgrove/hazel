[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Editor.EditState_Exp.t,
};

let mk = (info: CardInfo.t): t => {
  info,
  edit_state:
    Statics_Exp.fix_and_renumber_holes_z(Contexts.empty, info.init_zexp),
};
