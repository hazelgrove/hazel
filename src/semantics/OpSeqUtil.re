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
