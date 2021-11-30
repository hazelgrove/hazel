[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Statics.edit_state,
};

let mk = (info: CardInfo.t): t => {info, edit_state: info.init_edit_state};
