//let styp_to_htyp: Smyth.Lang.typ => option(HTyp.t);

type solve_result = list(list((MetaVar.t, UHExp.t)));

let solve: UHExp.t => option(solve_result);
