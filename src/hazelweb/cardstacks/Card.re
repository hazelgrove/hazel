[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Statics.edit_state,
};

let mk = (info: CardInfo.t): t => {
  info,
  edit_state: (info.init_zexp, HTyp.Int, 0),
  //Statics_Exp.fix_and_renumber_holes_z(Contexts.initial, info.init_zexp),
};
