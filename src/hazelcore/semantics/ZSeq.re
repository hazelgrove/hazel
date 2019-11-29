[@deriving sexp]
type t('operand, 'operator, 'zoperand, 'zoperator) =
  | ZOperand('zoperand, Seq.operand_surround('operand, 'operator))
  | ZOperator('zoperator, Seq.operator_surround('operand, 'operator));

let wrap = zoperand => ZOperand(zoperand, (E, E));

let erase =
    (
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zoperator: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : Seq.t('operand, 'operator) =>
  switch (zseq) {
  | ZOperand(zoperand, (prefix, suffix)) =>
    let operand = zoperand |> erase_zoperand;
    Seq.affix_seq(prefix, S(operand, suffix));
  | ZOperator(zoperator, (prefix, suffix)) =>
    let operator = zoperator |> erase_zoperator;
    Seq.seq_affix(prefix, A(operator, suffix));
  };

let is_before =
    (~is_before_zoperand: 'zoperand => bool, zseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zseq) {
  | ZOperator(_, _) => false
  | ZOperand(zoperand, _) => is_before_zoperand(zoperand)
  };

let is_after =
    (~is_after_zoperand: 'zoperand => bool, zseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zseq) {
  | ZOperator(_, _) => false
  | ZOperand(zoperand, _) => is_after_zoperand(zoperand)
  };

let place_before =
    (
      ~place_before_operand: 'operand => 'zoperand,
      seq: Seq.t('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let (first, suffix) = seq |> Seq.split_first_and_suffix;
  ZOperand(first |> place_before_operand, (E, suffix));
};

let place_after =
    (
      ~place_after_operand: 'operand => 'zoperand,
      seq: Seq.t('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let (prefix, last) = seq |> Seq.split_prefix_and_last;
  ZOperand(last |> place_after_operand, (prefix, E));
};

let move_cursor_left =
    (
      ~move_cursor_left_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_left_zoperator: 'zoperator => option('zoperator),
      ~place_after_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => 'zoperator,
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zoperator: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zseq) {
  | ZOperand(zoperand, surround) =>
    switch (move_cursor_left_zoperand(zoperand), surround) {
    | (None, (E, _)) => None
    | (None, (A(left_op, preseq), suffix)) =>
      let operand = zoperand |> erase_zoperand;
      let left_zop = left_op |> place_after_operator;
      Some(ZOperator(left_zop, (preseq, S(operand, suffix))));
    | (Some(zoperand), _) => Some(ZOperand(zoperand, surround))
    }
  | ZOperator(zop, surround) =>
    switch (move_cursor_left_zoperator(zop), surround) {
    | (None, (preseq, sufseq)) =>
      let op = zop |> erase_zoperator;
      let (prefix, left_operand) = preseq |> Seq.split_prefix_and_last;
      let left_zoperand = left_operand |> place_after_operand;
      Some(ZOperand(left_zoperand, (prefix, A(op, sufseq))));
    | (Some(zop), _) => Some(ZOperator(zop, surround))
    }
  };

let move_cursor_right =
    (
      ~move_cursor_right_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_right_zoperator: 'zoperator => option('zoperator),
      ~place_before_operand: 'operand => 'zoperand,
      ~place_before_operator: 'operator => 'zoperator,
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zoperator: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zseq) {
  | ZOperand(zoperand, surround) =>
    switch (move_cursor_right_zoperand(zoperand), surround) {
    | (None, (_, E)) => None
    | (None, (prefix, A(right_op, sufseq))) =>
      let operand = zoperand |> erase_zoperand;
      let right_zop = right_op |> place_before_operator;
      Some(ZOperator(right_zop, (S(operand, prefix), sufseq)));
    | (Some(zoperand), _) => Some(ZOperand(zoperand, surround))
    }
  | ZOperator(zop, surround) =>
    switch (move_cursor_right_zoperator(zop), surround) {
    | (None, (preseq, sufseq)) =>
      let op = zop |> erase_zoperator;
      let (right_operand, suffix) = sufseq |> Seq.split_first_and_suffix;
      let right_zoperand = right_operand |> place_before_operand;
      Some(ZOperand(right_zoperand, (A(op, preseq), suffix)));
    | (Some(zop), _) => Some(ZOperator(zop, surround))
    }
  };
