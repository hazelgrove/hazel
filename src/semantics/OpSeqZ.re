type t('operand, 'operator, 'zoperand, 'zoperator) =
  | ZOperand(
      OpSeq.skel('operator),
      'zoperand,
      operand_surround('operand, 'operator),
    )
  | ZOperator(
      OpSeq.skel('operator),
      'zoperator,
      operator_surround('operand, 'operator),
    )
and operand_surround('operand, 'operator) = (
  Seq.affix('operand, 'operator),
  Seq.affix('operand, 'operator),
)
and operator_surround('operand, 'operator) = (
  Seq.t('operand, 'operator),
  Seq.t('operand, 'operator),
);

let erase =
    (
      ~erase_operand: 'zoperand => 'operand,
      ~erase_operator: 'zoperator => 'operator,
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : OpSeq.t('operand, 'operator) =>
  switch (zopseq) {
  | ZOperand(skel, zoperand, (prefix, suffix)) =>
    let operand = zoperand |> erase_operand;
    let seq = Seq.prefix_seq(prefix, S(operand, suffix));
    OpSeq(skel, seq);
  | ZOperator(skel, zoperator, (prefix, suffix)) =>
    let operator = zoperator |> erase_operator;
    let seq = Seq.seq_suffix(prefix, A(operator, suffix));
    OpSeq(skel, seq);
  };

let is_before =
    (~is_before_operand: 'zoperand => bool, zopseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zopseq) {
  | ZOperator(_, _, _) => false
  | ZOperand(_, zoperand, _) => is_before_operand(zoperand)
  };

let is_after =
    (~is_after_operand: 'zoperand => bool, zopseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zopseq) {
  | ZOperator(_, _, _) => false
  | ZOperand(_, zoperand, _) => is_after_operand(zoperand)
  };

let place_before =
    (
      ~place_before_operand: 'operand => 'zoperand,
      OpSeq(skel, seq): OpSeq.t('operand, _),
    )
    : t('operand, 'zoperand, _, _) => {
  let (first, suffix) = seq |> Seq.split_first_and_suffix;
  OpSeqZ(skel, first |> place_before_operand, (E, suffix));
};

let place_after =
    (
      ~place_after_operand: 'operand => 'zoperand,
      OpSeq(skel, seq): OpSeq.t('operand, _),
    )
    : t('operand, 'zoperand, _, _) => {
  let (prefix, last) = seq |> Seq.split_prefix_and_last;
  OpSeqZ(skel, last |> place_after_operand, (prefix, E));
};
