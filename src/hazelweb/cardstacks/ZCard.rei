/**
 * The selected card in a Cardstack.
 */
[@deriving (sexp, show)]
type t = {
  info: CardInfo.t,
  program: Program.t,
};

let mk: (~width: int, Card.t) => t;

let erase: t => Card.t;

let get_program: t => Program.t;
let put_program: (Program.t, t) => t;
