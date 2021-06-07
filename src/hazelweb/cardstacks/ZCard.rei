/**
 * The selected card in a Cardstack.
 */

[@deriving sexp]
type t = {
  info: CardInfo.t,
  program: Program.exp,
};

let mk: (~width: int, Card.t) => t;

let erase: t => Card.t;

let get_program: t => Program.exp;
let put_program: (Program.exp, t) => t;
