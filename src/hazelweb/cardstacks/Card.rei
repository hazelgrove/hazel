/**
 * An unselected card in a Cardstack.
 */
[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Statics_common.edit_state,
};

let mk: CardInfo.t => t;
