open GeneralUtil;

let mk_OpSeq = (seq: UHPat.opseq): UHPat.t_inner => {
  let skel = Associator.associate_pat(seq);
  OpSeq(skel, seq);
};

let mk_OpSeqZ = (zp: ZPat.t, surround: ZPat.opseq_surround): ZPat.t => {
  let p = ZPat.erase(zp);
  let seq = OperatorSeq.opseq_of_exp_and_surround(p, surround);
  let skel = Associator.associate_pat(seq);
  OpSeqZ(skel, zp, surround);
};

let mk_Space_separated_prefix =
    (ps: list(UHPat.t)): option(ZPat.opseq_prefix) =>
  switch (ps) {
  | [] => None
  | [p] => Some(ExpPrefix(p, UHPat.Space))
  | [p1, p2, ...ps] =>
    let seq = OperatorSeq.join(ListMinTwo.mk(p1, p2, ps), UHPat.Space);
    Some(SeqPrefix(seq, UHPat.Space));
  };

let mk_Space_separated_suffix =
    (ps: list(UHPat.t)): option(ZPat.opseq_suffix) =>
  switch (ps) {
  | [] => None
  | [p] => Some(ExpSuffix(UHPat.Space, p))
  | [p1, p2, ...ps] =>
    let seq = OperatorSeq.join(ListMinTwo.mk(p1, p2, ps), UHPat.Space);
    Some(SeqSuffix(UHPat.Space, seq));
  };

let mk_Space_separated_zpat =
    (prefix_tms: list(UHPat.t), zp0: ZPat.t, suffix_tms: list(UHPat.t))
    : ZPat.t => {
  let opt_surround: option(ZPat.opseq_surround) =
    switch (
      mk_Space_separated_prefix(prefix_tms),
      mk_Space_separated_suffix(suffix_tms),
    ) {
    | (None, None) => None
    | (Some(prefix), None) => Some(EmptySuffix(prefix))
    | (None, Some(suffix)) => Some(EmptyPrefix(suffix))
    | (Some(prefix), Some(suffix)) => Some(BothNonEmpty(prefix, suffix))
    };
  switch (opt_surround) {
  | None => zp0
  | Some(surround) => mk_OpSeqZ(zp0, surround)
  };
};
