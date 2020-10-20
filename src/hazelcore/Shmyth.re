let htyp_to_styp = (_ty: UHTyp.t): Smyth.Lang.typ => {
  failwith(__LOC__);
};

let hexp_to_smexp = (_uhexp: UHExp.t): option(Smyth.Lang.exp) => {
  failwith(__LOC__);
};

let opseq_to_smexp = (_opseq: UHExp.opseq): option(Smyth.Lang.exp) => {
  failwith(__LOC__);
};

let str_to_smint = (_str: string): Smyth.Lang.exp => {
  failwith(__LOC__);
};

let operand_to_smexp = (operand: UHExp.operand): option(Smyth.Lang.exp) =>
  switch (operand) {
  | FloatLit(_)
  | BoolLit(_)
  | ApPalette(_)
  | InvalidText(_) => None
  | EmptyHole(number) => Some(EHole(number))
  | Var(_, _, name) => Some(EVar(name))
  | IntLit(_, str) => Some(str_to_smint(str))
  | ListNil(_) => failwith(__LOC__)
  | Lam(_, _var /*Var(_, _, param_name)*/, _ty, _body) => failwith(__LOC__)
  // separate non/annotated cases?
  // need seperate letline case to capture non-anonymous function
  | Inj(_, _side, _v) => failwith(__LOC__)
  | Case(_, _scrut, _rules) => failwith(__LOC__)
  | Parenthesized(expr) => hexp_to_smexp(expr) // hmmm
  };
