/**
 * An unselected card in a Cardstack.
 */
[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Program.EditState.t,
};

let mk: CardInfo.t => t;
