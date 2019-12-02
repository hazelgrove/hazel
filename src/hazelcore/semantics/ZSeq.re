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

// TODO prevent cursor on Space ops
let move_cursor_left =
    (
      ~move_cursor_left_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_left_zoperator: 'zoperator => option('zoperator),
      ~place_after_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => option('zoperator),
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zoperator: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zseq) {
  | ZOperand(zoperand, surround) =>
    switch (move_cursor_left_zoperand(zoperand), surround) {
    | (None, (E, _)) => None
    | (None, (A(prefix_hd, prefix_tl), suffix)) =>
      switch (prefix_hd |> place_after_operator) {
      | None =>
        let S(operand, new_prefix) = prefix_tl;
        let new_zoperand = operand |> place_after_operand;
        let new_suffix =
          Seq.A(prefix_hd, S(zoperand |> erase_zoperand, suffix));
        Some(ZOperand(new_zoperand, (new_prefix, new_suffix)));
      | Some(zoperator) =>
        let new_prefix = prefix_tl;
        let new_suffix = Seq.S(zoperand |> erase_zoperand, suffix);
        Some(ZOperator(zoperator, (new_prefix, new_suffix)));
      }
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
      ~place_before_operator: 'operator => option('zoperator),
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zoperator: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zseq) {
  | ZOperand(zoperand, surround) =>
    switch (move_cursor_right_zoperand(zoperand), surround) {
    | (None, (_, E)) => None
    | (None, (prefix, A(suffix_hd, suffix_tl))) =>
      switch (suffix_hd |> place_before_operator) {
      | None =>
        let S(operand, new_suffix) = suffix_tl;
        let new_zoperand = operand |> place_before_operand;
        let new_prefix =
          Seq.A(suffix_hd, S(zoperand |> erase_zoperand, prefix));
        Some(ZOperand(new_zoperand, (new_prefix, new_suffix)));
      | Some(zoperator) =>
        let new_prefix = Seq.S(zoperand |> erase_zoperand, prefix);
        let new_suffix = suffix_tl;
        Some(ZOperator(zoperator, (new_prefix, new_suffix)));
      }
    | (Some(zoperand), _) => Some(ZOperand(zoperand, surround))
    }
  | ZOperator(zop, surround) =>
    switch (move_cursor_right_zoperator(zop), surround) {
    | (None, (prefix, suffix)) =>
      let op = zop |> erase_zoperator;
      let new_prefix = Seq.A(op, prefix);
      let (operand, new_suffix) = suffix |> Seq.split_first_and_suffix;
      let zoperand = operand |> place_before_operand;
      Some(ZOperand(zoperand, (new_prefix, new_suffix)));
    | (Some(zop), _) => Some(ZOperator(zop, surround))
    }
  };
