open GeneralUtil;

type seq('operand, 'operator) = OpSeq.seq('operand, 'operator);
type operand_surround('operand, 'operator) =
  ZOpSeq.operand_surround('operand, 'operator);

let _shift_optm_from_prefix =
    (
      ~concat: ('operand, 'operator, 'operand) => 'operand,
      ~operand_surround: option(operand_surround('operand, 'operator)),
      body: 'operand,
    )
    : option(('operand, option(operand_surround('operand, 'operator)))) =>
  switch (operand_surround) {
  | None
  | Some(EmptyPrefix(_)) => None
  | Some(EmptySuffix(OperandPrefix(operand, op)))
  | Some(BothNonEmpty(OperandPrefix(operand, op), _)) =>
    let new_body = concat(operand, op, body);
    let new_surround =
      switch (operand_surround) {
      | Some(BothNonEmpty(_, suffix)) => Some(Seq.EmptyPrefix(suffix))
      | _ => None
      };
    Some((new_body, new_surround));
  | Some(EmptySuffix(SeqPrefix(seq, op)))
  | Some(BothNonEmpty(SeqPrefix(seq, op), _)) =>
    let (operand, new_prefix) = seq |> OpSeqSurround.split_prefix_and_last;
    let new_body = concat(operand, op, body);
    let new_surround =
      switch (operand_surround) {
      | Some(BothNonEmpty(_, suffix)) =>
        Some(Seq.BothNonEmpty(new_prefix, suffix))
      | _ => Some(EmptySuffix(new_prefix))
      };
    Some((new_body, new_surround));
  };

let _shift_optm_from_suffix =
    (
      ~concat: ('operand, 'operator, 'operand) => 'operand,
      ~operand_surround: option(operand_surround('operand, 'operator)),
      body: 'operand,
    )
    : option(('operand, option(operand_surround('operand, 'operator)))) =>
  switch (operand_surround) {
  | None
  | Some(EmptySuffix(_)) => None
  | Some(EmptyPrefix(OperandSuffix(op, operand)))
  | Some(BothNonEmpty(_, OperandSuffix(op, operand))) =>
    let new_body = concat(body, op, operand);
    let new_surround =
      switch (operand_surround) {
      | Some(BothNonEmpty(prefix, _)) => Some(Seq.EmptySuffix(prefix))
      | _ => None
      };
    Some((new_body, new_surround));
  | Some(EmptyPrefix(SeqSuffix(op, seq)))
  | Some(BothNonEmpty(_, SeqSuffix(op, seq))) =>
    let (operand, new_suffix) = seq |> OpSeqSurround.split_first_and_suffix;
    let new_body = concat(body, op, operand);
    let new_surround =
      switch (operand_surround) {
      | Some(BothNonEmpty(prefix, _)) =>
        Some(Seq.BothNonEmpty(prefix, new_suffix))
      | _ => Some(EmptyPrefix(new_suffix))
      };
    Some((new_body, new_surround));
  };

let _shift_optm_to_prefix =
    (
      ~is_OpSeq: 'operand => option(seq('operand, 'operator)),
      ~mk_OpSeq: seq('operand, 'operator) => 'operand,
      ~operand_surround: option(operand_surround('operand, 'operator)),
      body: 'operand,
    )
    : option(('operand, option(operand_surround('operand, 'operator)))) =>
  is_OpSeq(body)
  |> Opt.map(seq =>
       switch (seq |> OpSeqSurround.split_first_and_suffix) {
       | (first, OperandSuffix(op, operand)) => (
           operand,
           Some(
             switch (operand_surround) {
             | None => Seq.EmptySuffix(OperandPrefix(first, op))
             | Some(operand_surround) =>
               Seq.nest_surrounds(
                 EmptySuffix(OperandPrefix(first, op)),
                 operand_surround,
               )
             },
           ),
         )
       | (first, SeqSuffix(op, seq)) => (
           mk_OpSeq(seq),
           Some(
             switch (operand_surround) {
             | None => EmptySuffix(OperandPrefix(first, op))
             | Some(operand_surround) =>
               Seq.nest_surrounds(
                 EmptySuffix(OperandPrefix(first, op)),
                 operand_surround,
               )
             },
           ),
         )
       }
     );

let _shift_optm_to_suffix =
    (
      ~is_OpSeq: 'operand => option(seq('operand, 'operator)),
      ~mk_OpSeq: seq('operand, 'operator) => 'operand,
      ~operand_surround: option(operand_surround('operand, 'operator)),
      body: 'operand,
    )
    : option(('operand, option(operand_surround('operand, 'operator)))) =>
  is_OpSeq(body)
  |> Opt.map(seq =>
       switch (seq |> OpSeqSurround.split_prefix_and_last) {
       | (last, OperandPrefix(operand, op)) => (
           operand,
           Some(
             switch (operand_surround) {
             | None => Seq.EmptyPrefix(OperandSuffix(op, last))
             | Some(operand_surround) =>
               Seq.nest_surrounds(
                 EmptyPrefix(OperandSuffix(op, last)),
                 operand_surround,
               )
             },
           ),
         )
       | (last, SeqPrefix(seq, op)) => (
           mk_OpSeq(seq),
           Some(
             switch (operand_surround) {
             | None => EmptyPrefix(OperandSuffix(op, last))
             | Some(operand_surround) =>
               Seq.nest_surrounds(
                 EmptyPrefix(OperandSuffix(op, last)),
                 operand_surround,
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

  let mk_OpSeqZ = (zty: ZTyp.t, operand_surround: ZTyp.opseq_surround): ZTyp.t => {
    let seq =
      Seq.t_of_operand_and_surround(ZTyp.erase(zty), operand_surround);
    let skel = Associator.associate_ty(seq);
    OpSeqZ(skel, zty, operand_surround);
  };

  let concat = (ty1: UHTyp.t, op: UHTyp.operator, ty2: UHTyp.t): UHTyp.t =>
    mk_OpSeq(
      switch (ty1, ty2) {
      | (OpSeq(_, seq1), OpSeq(_, seq2)) => Seq.seq_op_seq(seq1, op, seq2)
      | (OpSeq(_, seq1), _) => SeqOpExp(seq1, op, ty2)
      | (_, OpSeq(_, seq2)) => Seq.operand_op_seq(ty1, op, seq2)
      | (_, _) => ExpOpExp(ty1, op, ty2)
      },
    );

  let resurround =
      (zty0: ZTyp.t, operand_surround: ZTyp.opseq_surround)
      : (ZTyp.t, ZTyp.opseq_surround) =>
    switch (zty0) {
    | OpSeqZ(_, zty0, inner_surround) => (
        zty0,
        Seq.nest_surrounds(inner_surround, operand_surround),
      )
    | _ => (zty0, operand_surround)
    };

  let shift_optm_from_prefix =
      (~operand_surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_from_prefix(~concat, ~operand_surround, body);

  let shift_optm_from_suffix =
      (~operand_surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_from_suffix(~concat, ~operand_surround, body);

  let shift_optm_to_prefix =
      (~operand_surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_to_prefix(~is_OpSeq, ~mk_OpSeq, ~operand_surround, body);

  let shift_optm_to_suffix =
      (~operand_surround: option(ZTyp.opseq_surround), body: UHTyp.t)
      : option((UHTyp.t, option(ZTyp.opseq_surround))) =>
    _shift_optm_to_suffix(~is_OpSeq, ~mk_OpSeq, ~operand_surround, body);
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

  let mk_OpSeqZ = (zp: ZPat.t, operand_surround: ZPat.opseq_surround): ZPat.t => {
    let seq =
      Seq.t_of_operand_and_surround(ZPat.erase(zp), operand_surround);
    let skel = Associator.associate_pat(seq);
    OpSeqZ(skel, zp, operand_surround);
  };

  let concat = (p1: UHPat.t, op: UHPat.operator, p2: UHPat.t): UHPat.t =>
    mk_OpSeq(
      switch (p1, p2) {
      | (OpSeq(_, seq1), OpSeq(_, seq2)) => Seq.seq_op_seq(seq1, op, seq2)
      | (OpSeq(_, seq1), _) => SeqOpExp(seq1, op, p2)
      | (_, OpSeq(_, seq2)) => Seq.operand_op_seq(p1, op, seq2)
      | (_, _) => ExpOpExp(p1, op, p2)
      },
    );

  let resurround =
      (zp0: ZPat.t, operand_surround: ZPat.opseq_surround)
      : (ZPat.t, ZPat.opseq_surround) =>
    switch (zp0) {
    | OpSeqZ(_, zp0, inner_surround) => (
        zp0,
        Seq.nest_surrounds(inner_surround, operand_surround),
      )
    | _ => (zp0, operand_surround)
    };

  let shift_optm_from_prefix =
      (~operand_surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_from_prefix(~concat, ~operand_surround, body);

  let shift_optm_from_suffix =
      (~operand_surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_from_suffix(~concat, ~operand_surround, body);

  let shift_optm_to_prefix =
      (~operand_surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_to_prefix(~is_OpSeq, ~mk_OpSeq, ~operand_surround, body);

  let shift_optm_to_suffix =
      (~operand_surround: option(ZPat.opseq_surround), body: UHPat.t)
      : option((UHPat.t, option(ZPat.opseq_surround))) =>
    _shift_optm_to_suffix(~is_OpSeq, ~mk_OpSeq, ~operand_surround, body);
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

  let mk_OpSeqZ = (ze: ZExp.t, operand_surround: ZExp.opseq_surround): ZExp.t => {
    let seq =
      Seq.t_of_operand_and_surround(
        ZExp.erase_zoperand(ze),
        operand_surround,
      );
    let skel = Associator.associate_exp(seq);
    OpSeqZ(skel, ze, operand_surround);
  };

  let concat = (e1: UHExp.t, op: UHExp.operator, e2: UHExp.t): UHExp.t =>
    mk_OpSeq(
      switch (e1, e2) {
      | (OpSeq(_, seq1), OpSeq(_, seq2)) => Seq.seq_op_seq(seq1, op, seq2)
      | (OpSeq(_, seq1), _) => SeqOpExp(seq1, op, e2)
      | (_, OpSeq(_, seq2)) => Seq.operand_op_seq(e1, op, seq2)
      | (_, _) => ExpOpExp(e1, op, e2)
      },
    );

  let prepend_seq =
      (seq: UHExp.opseq, op: UHExp.operator, e: UHExp.t): UHExp.t =>
    switch (e) {
    | OpSeq(_, seq') => Seq.seq_op_seq(seq, op, seq') |> mk_OpSeq
    | _ => Seq.SeqOpExp(seq, op, e) |> mk_OpSeq
    };

  let append_seq = (e: UHExp.t, op: UHExp.operator, seq: UHExp.opseq): UHExp.t =>
    switch (e) {
    | OpSeq(_, seq') => Seq.seq_op_seq(seq', op, seq) |> mk_OpSeq
    | _ => Seq.operand_op_seq(e, op, seq) |> mk_OpSeq
    };

  let prepend = (prefix: ZExp.prefix, e: UHExp.t): UHExp.t =>
    switch (prefix) {
    | OperandPrefix(operand, op) => concat(operand, op, e)
    | SeqPrefix(seq, op) => prepend_seq(seq, op, e)
    };

  let append = (e: UHExp.t, suffix: ZExp.suffix): UHExp.t =>
    switch (suffix) {
    | OperandSuffix(op, operand) => concat(e, op, operand)
    | SeqSuffix(op, seq) => append_seq(e, op, seq)
    };

  let resurround =
      (ze0: ZExp.t, operand_surround: ZExp.opseq_surround)
      : (ZExp.t, ZExp.opseq_surround) =>
    switch (ze0) {
    | OpSeqZ(_, ze0, inner_surround) => (
        ze0,
        Seq.nest_surrounds(inner_surround, operand_surround),
      )
    | _ => (ze0, operand_surround)
    };
  let shift_optm_from_prefix =
      (~operand_surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_from_prefix(~concat, ~operand_surround, body);

  let shift_optm_from_suffix =
      (~operand_surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_from_suffix(~concat, ~operand_surround, body);

  let shift_optm_to_prefix =
      (~operand_surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_to_prefix(~is_OpSeq, ~mk_OpSeq, ~operand_surround, body);

  let shift_optm_to_suffix =
      (~operand_surround: option(ZExp.opseq_surround), body: UHExp.t)
      : option((UHExp.t, option(ZExp.opseq_surround))) =>
    _shift_optm_to_suffix(~is_OpSeq, ~mk_OpSeq, ~operand_surround, body);
};
