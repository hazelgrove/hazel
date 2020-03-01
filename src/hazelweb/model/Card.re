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
let put_zexp = (ze: ZExp.t, card: t): t => {
  let new_info = card.info |> CardInfo.put_zexp(ze);
  mk(new_info);
};
let get_program = card => card.program;

let put_program = (program: Program.t, card: t): t => {...card, program};
