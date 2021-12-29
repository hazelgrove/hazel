[@deriving sexp]
type t = {
  info: CardInfo.t,
  prelude: Statics.edit_state,
  template: Statics.edit_state,
  tester: Statics.edit_state,
};

// combine the edit states
let mk = (info: CardInfo.t): t => {
  info,
  prelude: info.init_template,
  template: info.init_template,
  tester: info.init_template,
};
