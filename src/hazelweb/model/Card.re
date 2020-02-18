type t = {
  info: CardInfo.t,
  program: Program.t,
};

let mk = (info: CardInfo.t): t => {
  info,
  program:
    Program.mk(
      Statics.Exp.fix_and_renumber_holes_z(Contexts.empty, info.init_zexp),
    ),
};

let get_program = card => card.program;

let put_program = (program: Program.t, card: t): t => {...card, program};
