[@deriving sexp]
type call_complete =
  | Complete
  | Indet
  | Both;

[@deriving sexp]
type call_complete_context = VarMap.t_(call_complete);

[@deriving sexp]
type name_context = VarMap.t_((Var.t, Var.t));

let analyze_calls_prog = (_prog: Anf.prog, cctx) => {
  cctx;
};

let analyze_fix_prog = (prog: Anf.prog, _cctx, _nctx) => {
  prog;
};

let analyze = (prog: Anf.prog): Anf.prog => {
  let cctx = analyze_calls_prog(prog, VarMap.empty);
  let prog = analyze_fix_prog(prog, cctx, VarMap.empty);

  prog;
};
