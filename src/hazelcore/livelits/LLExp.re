open OptUtil.Syntax;

type meta =
  | Free(MetaVar.t)
  | Ap({
      u: MetaVar.t,
      err: ErrStatus.t,
      base_lln: LivelitName.t,
      model: SerializedModel.t,
      splice_info: UHExp.splice_info,
    });

type t = {
  meta,
  hd: LivelitName.t,
  args: list(UHExp.operand),
};

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
  let+ (hd, meta) =
    switch (hd) {
    | ApLivelit(u, err, base_lln, lln, model, splice_info) =>
      Some((lln, Ap({u, err, base_lln, model, splice_info})))
    | FreeLivelit(u, lln) => Some((lln, Free(u)))
    | _ => None
    };
  {meta, hd, args};
};

let to_uhexp = ({meta, hd, args}: t): UHExp.t => {
  let hd =
    switch (meta) {
    | Free(u) => UHExp.FreeLivelit(u, hd)
    | Ap({u, err, base_lln, model, splice_info}) =>
      ApLivelit(u, err, base_lln, hd, model, splice_info)
    };
  let seq = Seq.mk(hd, List.map(arg => (Operators_Exp.Space, arg), args));
  [ExpLine(UHExp.mk_OpSeq(seq))];
};
