module Typ = {
  let mk_OpSeq = (seq: UHTyp.opseq): UHTyp.t => {
    let skel = Associator.associate_ty(seq);
    OpSeq(skel, seq);
  };

  let mk_OpSeqZ = (zty: ZTyp.t, surround: ZTyp.opseq_surround): ZTyp.t => {
    let seq =
      OperatorSeq.opseq_of_exp_and_surround(ZTyp.erase(zty), surround);
    let skel = Associator.associate_ty(seq);
    OpSeqZ(skel, zty, surround);
  };
  let concat = (ty1: UHTyp.t, op: UHTyp.op, ty2: UHTyp.t): UHTyp.t =>
    mk_OpSeq(
      switch (ty1, ty2) {
      | (OpSeq(_, seq1), OpSeq(_, seq2)) =>
        OperatorSeq.seq_op_seq(seq1, op, seq2)
      | (OpSeq(_, seq1), _) => SeqOpExp(seq1, op, ty2)
      | (_, OpSeq(_, seq2)) => OperatorSeq.exp_op_seq(ty1, op, seq2)
      | (_, _) => ExpOpExp(ty1, op, ty2)
      },
    );
  let resurround =
      (zty0: ZTyp.t, surround: ZTyp.opseq_surround)
      : (ZTyp.t, ZTyp.opseq_surround) =>
    switch (zty0) {
    | OpSeqZ(_, zty0, inner_surround) => (
        zty0,
        OperatorSeq.nest_surrounds(inner_surround, surround),
      )
    | _ => (zty0, surround)
    };
  let shift_optm_to_suffix =
      (~surround: option(ZTyp.opseq_surround), ty: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    switch (ty) {
    | Hole
    | Unit
    | Num
    | Bool
    | Parenthesized(_)
    | List(_) => None
    | OpSeq(_, seq) =>
      switch (seq |> OperatorSeq.split_tail) {
      | (last, ExpPrefix(tm, op)) =>
        Some((
          tm,
          Some(
            switch (surround) {
            | None => EmptyPrefix(ExpSuffix(op, last))
            | Some(surround) =>
              OperatorSeq.nest_surrounds(
                EmptyPrefix(ExpSuffix(op, last)),
                surround,
              )
            },
          ),
        ))
      | (last, SeqPrefix(seq, op)) =>
        Some((
          mk_OpSeq(seq),
          Some(
            switch (surround) {
            | None => EmptyPrefix(ExpSuffix(op, last))
            | Some(surround) =>
              OperatorSeq.nest_surrounds(
                EmptyPrefix(ExpSuffix(op, last)),
                surround,
              )
            },
          ),
        ))
      }
    };

  /* TODO */
  [@warning "-27"]
  let shift_optm_from_suffix =
      (~surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    None;

  /* TODO */
  [@warning "-27"]
  let shift_optm_to_prefix =
      (~surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    None;

  let shift_optm_from_prefix =
      (~surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    switch (surround) {
    | None
    | Some(EmptyPrefix(_)) => None
    | Some(EmptySuffix(ExpPrefix(tm, op)))
    | Some(BothNonEmpty(ExpPrefix(tm, op), _)) =>
      let new_body = concat(tm, op, body);
      let new_surround =
        switch (surround) {
        | Some(BothNonEmpty(_, suffix)) =>
          Some(OperatorSeq.EmptyPrefix(suffix))
        | _ => None
        };
      Some((new_body, new_surround));
    | Some(EmptySuffix(SeqPrefix(seq, op)))
    | Some(BothNonEmpty(SeqPrefix(seq, op), _)) =>
      let (tm, new_prefix) = seq |> OperatorSeq.split_tail;
      let new_body = concat(tm, op, body);
      let new_surround =
        switch (surround) {
        | Some(BothNonEmpty(_, suffix)) =>
          Some(OperatorSeq.BothNonEmpty(new_prefix, suffix))
        | _ => Some(EmptySuffix(new_prefix))
        };
      Some((new_body, new_surround));
    };
};

module Pat = {
  let mk_OpSeq = (seq: UHPat.opseq): UHPat.t => {
    let skel = Associator.associate_pat(seq);
    OpSeq(skel, seq);
  };
  let mk_OpSeqZ = (zp: ZPat.t, surround: ZPat.opseq_surround): ZPat.t => {
    let seq =
      OperatorSeq.opseq_of_exp_and_surround(ZPat.erase(zp), surround);
    let skel = Associator.associate_pat(seq);
    OpSeqZ(skel, zp, surround);
  };
  let concat = (p1: UHPat.t, op: UHPat.op, p2: UHPat.t): UHPat.t =>
    mk_OpSeq(
      switch (p1, p2) {
      | (OpSeq(_, seq1), OpSeq(_, seq2)) =>
        OperatorSeq.seq_op_seq(seq1, op, seq2)
      | (OpSeq(_, seq1), _) => SeqOpExp(seq1, op, p2)
      | (_, OpSeq(_, seq2)) => OperatorSeq.exp_op_seq(p1, op, seq2)
      | (_, _) => ExpOpExp(p1, op, p2)
      },
    );

  let resurround =
      (zp0: ZPat.t, surround: ZPat.opseq_surround)
      : (ZPat.t, ZPat.opseq_surround) =>
    switch (zp0) {
    | OpSeqZ(_, zp0, inner_surround) => (
        zp0,
        OperatorSeq.nest_surrounds(inner_surround, surround),
      )
    | _ => (zp0, surround)
    };
};

module Exp = {
  let mk_OpSeq = (seq: UHExp.opseq): UHExp.t => {
    let skel = Associator.associate_exp(seq);
    OpSeq(skel, seq);
  };

  let mk_OpSeqZ = (ze: ZExp.t, surround: ZExp.opseq_surround): ZExp.t => {
    let seq =
      OperatorSeq.opseq_of_exp_and_surround(ZExp.erase(ze), surround);
    let skel = Associator.associate_exp(seq);
    OpSeqZ(skel, ze, surround);
  };

  let resurround =
      (ze0: ZExp.t, surround: ZExp.opseq_surround)
      : (ZExp.t, ZExp.opseq_surround) =>
    switch (ze0) {
    | OpSeqZ(_, ze0, inner_surround) => (
        ze0,
        OperatorSeq.nest_surrounds(inner_surround, surround),
      )
    | _ => (ze0, surround)
    };
};
