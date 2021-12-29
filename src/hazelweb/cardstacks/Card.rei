/**
 * An unselected card in a Cardstack.
 */
[@deriving sexp]
type t = {
  info: CardInfo.t,
  prelude: Statics.edit_state,
  template: Statics.edit_state,
  tester: Statics.edit_state,
};

let mk: CardInfo.t => t;
