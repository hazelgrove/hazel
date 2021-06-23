open OptUtil.Syntax;

type t = (LivelitName.t, list(UHExp.operand));

let rec get_spaced_args = (affix: Seq.affix(_, UHExp.operator)) =>
  switch (affix) {
  | E => Some([])
  | A(Space, S(arg, affix)) =>
    let+ args = get_spaced_args(affix);
    [arg, ...args];
  | A(_) => None
  };

let of_uhexp = (e: UHExp.t): option(t) => {
  let* (_, OpSeq(_, S(hd, tl))) = UHExp.Block.split_conclusion(e);
  let* args = get_spaced_args(tl);
  let+ hd =
    switch (hd) {
    | Var(_, _, lln) => Some(lln)
    | _ => None
    };
  (hd, args);
};

let to_uhexp = ((lln, args): t): UHExp.t => {
  let seq =
    Seq.mk(
      UHExp.var(lln),
      List.map(arg => (Operators_Exp.Space, arg), args),
    );
  [ExpLine(UHExp.mk_OpSeq(seq))];
};
