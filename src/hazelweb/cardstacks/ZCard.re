[@deriving sexp]
type t = {
  info: CardInfo.t,
  program: Program.t,
};

let mk = (~width, card: Card.t) => {
  info: card.info,
  // TODO: this should take in the whole program, not just the template
  program: Program.mk(~width, ~is_focused=true, card.template),
};

let erase = (zcard: t): Card.t => {
  info: zcard.info,
  // TODO: this should be the prelude
  prelude: zcard.program.edit_state,
  template: zcard.program.edit_state,
  // TODO: this should be the tester
  tester: zcard.program.edit_state,
};

let get_program = card => card.program;

let put_program = (program: Program.t, zcard: t): t => {...zcard, program};
