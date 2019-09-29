[@deriving sexp]
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
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let (first, suffix) = seq |> Seq.split_first_and_suffix;
  ZOperand(skel, first |> place_before_operand, (E, suffix));
};

let place_after =
    (
      ~place_after_operand: 'operand => 'zoperand,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let (prefix, last) = seq |> Seq.split_prefix_and_last;
  ZOperand(skel, last |> place_after_operand, (prefix, E));
};

let move_cursor_left =
    (
      ~move_cursor_left_operand: 'zoperand => option('zoperand),
      ~move_cursor_left_operator: 'zoperator => option('zoperator),
      ~place_after_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => 'zoperator,
      ~erase_operand: 'zoperand => 'operand,
      ~erase_operator: 'zoperator => 'operator,
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zopseq) {
  | ZOperand(skel, zoperand, surround) =>
    switch (move_cursor_left_operand(zoperand), surround) {
    | (None, (E, _)) => None
    | (None, (A(left_op, preseq), suffix)) =>
      let operand = zoperand |> erase_operand;
      let left_zop = left_op |> place_after_operator;
      Some(ZOperator(skel, left_zop, (preseq, S(operand, suffix))));
    | (Some(zoperand), _) => Some(ZOperand(skel, zoperand, surround))
    }
  | ZOperator(skel, zop, surround) =>
    switch (move_cursor_left_operator(zop), surround) {
    | (None, (preseq, sufseq)) =>
      let op = zop |> erase_operator;
      let (prefix, left_operand) = preseq |> Seq.split_prefix_and_last;
      let left_zoperand = left_operand |> place_after_operand;
      Some(ZOperand(skel, left_zoperand, (prefix, A(op, sufseq))));
    | (Some(zop), _) => Some(ZOperator(skel, zop, surround))
    }
  };

let move_cursor_right =
    (
      ~move_cursor_right_operand: 'zoperand => option('zoperand),
      ~move_cursor_right_operator: 'zoperator => option('zoperator),
      ~place_before_operand: 'operand => 'zoperand,
      ~place_before_operator: 'operator => 'zoperator,
      ~erase_operand: 'zoperand => 'operand,
      ~erase_operator: 'zoperator => 'operator,
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zopseq) {
  | ZOperand(skel, zoperand, surround) =>
    switch (move_cursor_right_operand(zoperand), surround) {
    | (None, (_, E)) => None
    | (None, (prefix, A(right_op, sufseq))) =>
      let operand = zoperand |> erase_operand;
      let right_zop = right_op |> place_before_operator;
      Some(ZOperator(skel, right_zop, (S(operand, prefix), sufseq)));
    | (Some(zoperand), _) => Some(ZOperand(skel, zoperand, surround))
    }
  | ZOperator(skel, zop, surround) =>
    switch (move_cursor_right_operator(zop), surround) {
    | (None, (preseq, sufseq)) =>
      let op = zop |> erase_operator;
      let (right_operand, suffix) = sufseq |> Seq.split_first_and_suffix;
      let right_zoperand = right_operand |> place_before_operand;
      Some(ZOperand(skel, right_zoperand, (A(op, preseq), suffix)));
    | (Some(zop), _) => Some(ZOperator(skel, zop, surround))
    }
  };
