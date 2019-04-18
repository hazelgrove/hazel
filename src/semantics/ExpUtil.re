let mk_OpSeq = (seq: UHExp.opseq): UHExp.t_inner => {
  let skel = Associator.associate_exp(seq);
  OpSeq(skel, seq);
};

let mk_OpSeqZ = (ze: ZExp.t, surround: ZExp.opseq_surround): ZExp.t => {
  let e = ZExp.erase(ze);
  let seq = OperatorSeq.opseq_of_exp_and_surround(e, surround);
  let skel = Associator.associate_exp(seq);
  OpSeqZ(skel, ze, surround);
};
