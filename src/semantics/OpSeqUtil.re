let opseq_typ = (seq: UHTyp.opseq): UHTyp.t => {
  let skel = Associator.associate_ty(seq);
  OpSeq(skel, seq);
};
let opseq_pat = (seq: UHPat.opseq): UHPat.t => {
  let skel = Associator.associate_pat(seq);
  OpSeq(skel, seq);
};
let opseq_exp = (seq: UHExp.opseq): UHExp.t => {
  let skel = Associator.associate_exp(seq);
  OpSeq(skel, seq);
};

let opseqz_typ = (zty: ZTyp.t, surround: ZTyp.opseq_surround): ZTyp.t => {
  let seq = OperatorSeq.opseq_of_exp_and_surround(ZTyp.erase(zty), surround);
  let skel = Associator.associate_ty(seq);
  OpSeqZ(skel, zty, surround);
};
let opseqz_pat = (zp: ZPat.t, surround: ZPat.opseq_surround): ZPat.t => {
  let seq = OperatorSeq.opseq_of_exp_and_surround(ZPat.erase(zp), surround);
  let skel = Associator.associate_pat(seq);
  OpSeqZ(skel, zp, surround);
};
let opseqz_exp = (ze: ZExp.t, surround: ZExp.opseq_surround): ZExp.t => {
  let seq = OperatorSeq.opseq_of_exp_and_surround(ZExp.erase(ze), surround);
  let skel = Associator.associate_exp(seq);
  OpSeqZ(skel, ze, surround);
};

let resurround_typ =
    (zty0: ZTyp.t, surround: ZTyp.opseq_surround)
    : (ZTyp.t, ZTyp.opseq_surround) =>
  switch (zty0) {
  | OpSeqZ(_, zty0, inner_surround) => (
      zty0,
      OperatorSeq.nest_surrounds(inner_surround, surround),
    )
  | _ => (zty0, surround)
  };
let resurround_pat =
    (zp0: ZPat.t, surround: ZPat.opseq_surround)
    : (ZPat.t, ZPat.opseq_surround) =>
  switch (zp0) {
  | OpSeqZ(_, zp0, inner_surround) => (
      zp0,
      OperatorSeq.nest_surrounds(inner_surround, surround),
    )
  | _ => (zp0, surround)
  };
let resurround_exp =
    (ze0: ZExp.t, surround: ZExp.opseq_surround)
    : (ZExp.t, ZExp.opseq_surround) =>
  switch (ze0) {
  | OpSeqZ(_, ze0, inner_surround) => (
      ze0,
      OperatorSeq.nest_surrounds(inner_surround, surround),
    )
  | _ => (ze0, surround)
  };
