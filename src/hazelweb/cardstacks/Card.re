[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Statics.edit_state,
};

let mk = (info: CardInfo.t): option(t) => {
  Option.map(
    edit_state => {info, edit_state},
    Statics_Exp.fix_and_renumber_holes_z(Contexts.empty, info.init_zexp),
  );
};
