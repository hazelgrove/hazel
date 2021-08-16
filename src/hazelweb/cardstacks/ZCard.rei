/**
 * The selected card in a Cardstack.
 */

[@deriving sexp]
type t = {
  info: CardInfo.t,
  program: Editor.exp,
};

let mk: (~width: int, Card.t) => t;

let erase: t => Card.t;

let get_program: t => Editor.exp;
let put_program: (Editor.exp, t) => t;
