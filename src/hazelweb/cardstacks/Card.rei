/**
 * An unselected card in a Cardstack.
 */
[@deriving sexp]
type t = {
  info: CardInfo.t,
  edit_state: Program.EditState_Exp.t,
};

let mk: CardInfo.t => t;
