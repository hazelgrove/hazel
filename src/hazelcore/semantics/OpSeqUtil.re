open GeneralUtil;

type seq('tm, 'op) = OperatorSeq.opseq('tm, 'op);
type surround('tm, 'op) = OperatorSeq.opseq_surround('tm, 'op);

let _shift_optm_from_prefix =
    (
      ~concat: ('tm, 'op, 'tm) => 'tm,
      ~surround: option(surround('tm, 'op)),
      body: 'tm,
    )
    : option(('tm, option(surround('tm, 'op)))) =>
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

let _shift_optm_from_suffix =
    (
      ~concat: ('tm, 'op, 'tm) => 'tm,
      ~surround: option(surround('tm, 'op)),
      body: 'tm,
    )
    : option(('tm, option(surround('tm, 'op)))) =>
  switch (surround) {
  | None
  | Some(EmptySuffix(_)) => None
  | Some(EmptyPrefix(ExpSuffix(op, tm)))
  | Some(BothNonEmpty(_, ExpSuffix(op, tm))) =>
    let new_body = concat(body, op, tm);
    let new_surround =
      switch (surround) {
      | Some(BothNonEmpty(prefix, _)) =>
        Some(OperatorSeq.EmptySuffix(prefix))
      | _ => None
      };
    Some((new_body, new_surround));
  | Some(EmptyPrefix(SeqSuffix(op, seq)))
  | Some(BothNonEmpty(_, SeqSuffix(op, seq))) =>
    let (tm, new_suffix) = seq |> OperatorSeq.split0;
    let new_body = concat(body, op, tm);
    let new_surround =
      switch (surround) {
      | Some(BothNonEmpty(prefix, _)) =>
        Some(OperatorSeq.BothNonEmpty(prefix, new_suffix))
      | _ => Some(EmptyPrefix(new_suffix))
      };
    Some((new_body, new_surround));
  };

let _shift_optm_to_prefix =
    (
      ~is_OpSeq: 'tm => option(seq('tm, 'op)),
      ~mk_OpSeq: seq('tm, 'op) => 'tm,
      ~surround: option(surround('tm, 'op)),
      body: 'tm,
    )
    : option(('tm, option(surround('tm, 'op)))) =>
  is_OpSeq(body)
  |> Opt.map(seq =>
       switch (seq |> OperatorSeq.split0) {
       | (first, ExpSuffix(op, tm)) => (
           tm,
           Some(
             switch (surround) {
             | None => OperatorSeq.EmptySuffix(ExpPrefix(first, op))
             | Some(surround) =>
               OperatorSeq.nest_surrounds(
                 EmptySuffix(ExpPrefix(first, op)),
                 surround,
               )
             },
           ),
         )
       | (first, SeqSuffix(op, seq)) => (
           mk_OpSeq(seq),
           Some(
             switch (surround) {
             | None => EmptySuffix(ExpPrefix(first, op))
             | Some(surround) =>
               OperatorSeq.nest_surrounds(
                 EmptySuffix(ExpPrefix(first, op)),
                 surround,
               )
             },
           ),
         )
       }
     );

let _shift_optm_to_suffix =
    (
      ~is_OpSeq: 'tm => option(seq('tm, 'op)),
      ~mk_OpSeq: seq('tm, 'op) => 'tm,
      ~surround: option(surround('tm, 'op)),
      body: 'tm,
    )
    : option(('tm, option(surround('tm, 'op)))) =>
  is_OpSeq(body)
  |> Opt.map(seq =>
       switch (seq |> OperatorSeq.split_tail) {
       | (last, ExpPrefix(tm, op)) => (
           tm,
           Some(
             switch (surround) {
             | None => OperatorSeq.EmptyPrefix(ExpSuffix(op, last))
             | Some(surround) =>
               OperatorSeq.nest_surrounds(
                 EmptyPrefix(ExpSuffix(op, last)),
                 surround,
               )
             },
           ),
         )
       | (last, SeqPrefix(seq, op)) => (
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
         )
       }
     );

module Typ = {
  let is_OpSeq =
    fun
    | UHTyp.OpSeq(_, seq) => Some(seq)
    | _ => None;

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

  let shift_optm_from_prefix =
      (~surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_from_prefix(~concat, ~surround, body);

  let shift_optm_from_suffix =
      (~surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_from_suffix(~concat, ~surround, body);

  let shift_optm_to_prefix =
      (~surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_to_prefix(~is_OpSeq, ~mk_OpSeq, ~surround, body);

  let shift_optm_to_suffix =
      (~surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_to_suffix(~is_OpSeq, ~mk_OpSeq, ~surround, body);
};

module Pat = {
  let is_OpSeq =
    fun
    | UHPat.OpSeq(_, seq) => Some(seq)
    | _ => None;

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

  let shift_optm_from_prefix =
      (~surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_from_prefix(~concat, ~surround, body);

  let shift_optm_from_suffix =
      (~surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_from_suffix(~concat, ~surround, body);

  let shift_optm_to_prefix =
      (~surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_to_prefix(~is_OpSeq, ~mk_OpSeq, ~surround, body);

  let shift_optm_to_suffix =
      (~surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_to_suffix(~is_OpSeq, ~mk_OpSeq, ~surround, body);
};

module Exp = {
  let is_OpSeq =
    fun
    | UHExp.OpSeq(_, seq) => Some(seq)
    | _ => None;

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

  let concat = (e1: UHExp.t, op: UHExp.op, e2: UHExp.t): UHExp.t =>
    mk_OpSeq(
      switch (e1, e2) {
      | (OpSeq(_, seq1), OpSeq(_, seq2)) =>
        OperatorSeq.seq_op_seq(seq1, op, seq2)
      | (OpSeq(_, seq1), _) => SeqOpExp(seq1, op, e2)
      | (_, OpSeq(_, seq2)) => OperatorSeq.exp_op_seq(e1, op, seq2)
      | (_, _) => ExpOpExp(e1, op, e2)
      },
    );

  let prepend_seq = (seq: UHExp.opseq, op: UHExp.op, e: UHExp.t): UHExp.t =>
    switch (e) {
    | OpSeq(_, seq') => OperatorSeq.seq_op_seq(seq, op, seq') |> mk_OpSeq
    | _ => OperatorSeq.SeqOpExp(seq, op, e) |> mk_OpSeq
    };

  let append_seq = (e: UHExp.t, op: UHExp.op, seq: UHExp.opseq): UHExp.t =>
    switch (e) {
    | OpSeq(_, seq') => OperatorSeq.seq_op_seq(seq', op, seq) |> mk_OpSeq
    | _ => OperatorSeq.exp_op_seq(e, op, seq) |> mk_OpSeq
    };

  let prepend = (prefix: ZExp.opseq_prefix, e: UHExp.t): UHExp.t =>
    switch (prefix) {
    | ExpPrefix(tm, op) => concat(tm, op, e)
    | SeqPrefix(seq, op) => prepend_seq(seq, op, e)
    };

  let append = (e: UHExp.t, suffix: ZExp.opseq_suffix): UHExp.t =>
    switch (suffix) {
    | ExpSuffix(op, tm) => concat(e, op, tm)
    | SeqSuffix(op, seq) => append_seq(e, op, seq)
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
  let shift_optm_from_prefix =
      (~surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_from_prefix(~concat, ~surround, body);

  let shift_optm_from_suffix =
      (~surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_from_suffix(~concat, ~surround, body);

  let shift_optm_to_prefix =
      (~surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_to_prefix(~is_OpSeq, ~mk_OpSeq, ~surround, body);

  let shift_optm_to_suffix =
      (~surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_to_suffix(~is_OpSeq, ~mk_OpSeq, ~surround, body);
};
