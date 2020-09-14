/**
 * An unselected card in a Cardstack.
 */
[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Statics.edit_state,
};

let mk: CardInfo.t => t;
