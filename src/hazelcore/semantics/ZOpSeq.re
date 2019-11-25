[@deriving sexp]
type t('operand, 'operator, 'zoperand, 'zoperator) =
  | ZOperand(
      OpSeq.skel('operator),
      'zoperand,
      Seq.operand_surround('operand, 'operator),
    )
  | ZOperator(
      OpSeq.skel('operator),
      'zoperator,
      Seq.operator_surround('operand, 'operator),
    );

let mk_ZOperand =
    (
      ~associate: Seq.t('operand, 'operator) => Skel.t('operator),
      ~erase_zoperand: 'zoperand => 'operand,
      zoperand: 'zoperand,
      surround: Seq.operand_surround('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let seq =
    Seq.t_of_operand_and_surround(erase_zoperand(zoperand), surround);
  let skel = associate(seq);
  ZOperand(skel, zoperand, surround);
};

let mk_ZOperator =
    (
      ~associate: Seq.t('operand, 'operator) => Skel.t('operator),
      ~erase_zoperator: 'zoperator => 'operator,
      zoperator: 'zoperator,
      surround: Seq.operator_surround('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let seq =
    Seq.t_of_operator_and_surround(erase_zoperator(zoperator), surround);
  let skel = associate(seq);
  ZOperator(skel, zoperator, surround);
};

let wrap = zoperand => ZOperand(Placeholder(0), zoperand, (E, E));

let set_err_status =
    (
      ~set_err_status_zoperand: (ErrStatus.t, 'zoperand) => 'zoperand,
      err: ErrStatus.t,
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) =>
  switch (zopseq) {
  | ZOperator(Placeholder(_), _, _) => assert(false)
  | ZOperator(BinOp(_, op, skel1, skel2), zop, surround) =>
    ZOperator(BinOp(err, op, skel1, skel2), zop, surround)
  | ZOperand(Placeholder(_) as skel, zoperand, surround) =>
    ZOperand(skel, zoperand |> set_err_status_zoperand(err), surround)
  | ZOperand(BinOp(_, op, skel1, skel2), zoperand, surround) =>
    ZOperand(BinOp(err, op, skel1, skel2), zoperand, surround)
  };

let make_inconsistent =
    (
      ~make_inconsistent_zoperand:
         (MetaVarGen.t, 'zoperand) => ('zoperand, MetaVarGen.t),
      u_gen: MetaVarGen.t,
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : (t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t) =>
  switch (zopseq) {
  | ZOperator(Placeholder(_), _, _) => assert(false)
  | ZOperator(BinOp(_, op, skel1, skel2), zop, surround) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    (
      ZOperator(
        BinOp(InHole(TypeInconsistent, u), op, skel1, skel2),
        zop,
        surround,
      ),
      u_gen,
    );
  | ZOperand(Placeholder(_) as skel, zoperand, surround) =>
    let (zoperand, u_gen) = zoperand |> make_inconsistent_zoperand(u_gen);
    (ZOperand(skel, zoperand, surround), u_gen);
  | ZOperand(BinOp(_, op, skel1, skel2), zoperand, surround) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    (
      ZOperand(
        BinOp(InHole(TypeInconsistent, u), op, skel1, skel2),
        zoperand,
        surround,
      ),
      u_gen,
    );
  };

let erase =
    (
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zoperator: 'zoperator => 'operator,
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : OpSeq.t('operand, 'operator) =>
  switch (zopseq) {
  | ZOperand(skel, zoperand, (prefix, suffix)) =>
    let operand = zoperand |> erase_zoperand;
    let seq = Seq.prefix_seq(prefix, S(operand, suffix));
    OpSeq(skel, seq);
  | ZOperator(skel, zoperator, (prefix, suffix)) =>
    let operator = zoperator |> erase_zoperator;
    let seq = Seq.seq_suffix(prefix, A(operator, suffix));
    OpSeq(skel, seq);
  };

let is_before =
    (~is_before_zoperand: 'zoperand => bool, zopseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zopseq) {
  | ZOperator(_, _, _) => false
  | ZOperand(_, zoperand, _) => is_before_zoperand(zoperand)
  };

let is_after =
    (~is_after_zoperand: 'zoperand => bool, zopseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zopseq) {
  | ZOperator(_, _, _) => false
  | ZOperand(_, zoperand, _) => is_after_zoperand(zoperand)
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
      ~move_cursor_left_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_left_zoperator: 'zoperator => option('zoperator),
      ~place_after_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => 'zoperator,
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zoperator: 'zoperator => 'operator,
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zopseq) {
  | ZOperand(skel, zoperand, surround) =>
    switch (move_cursor_left_zoperand(zoperand), surround) {
    | (None, (E, _)) => None
    | (None, (A(left_op, preseq), suffix)) =>
      let operand = zoperand |> erase_zoperand;
      let left_zop = left_op |> place_after_operator;
      Some(ZOperator(skel, left_zop, (preseq, S(operand, suffix))));
    | (Some(zoperand), _) => Some(ZOperand(skel, zoperand, surround))
    }
  | ZOperator(skel, zop, surround) =>
    switch (move_cursor_left_zoperator(zop), surround) {
    | (None, (preseq, sufseq)) =>
      let op = zop |> erase_zoperator;
      let (prefix, left_operand) = preseq |> Seq.split_prefix_and_last;
      let left_zoperand = left_operand |> place_after_operand;
      Some(ZOperand(skel, left_zoperand, (prefix, A(op, sufseq))));
    | (Some(zop), _) => Some(ZOperator(skel, zop, surround))
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
      zopseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zopseq) {
  | ZOperand(skel, zoperand, surround) =>
    switch (move_cursor_right_zoperand(zoperand), surround) {
    | (None, (_, E)) => None
    | (None, (prefix, A(right_op, sufseq))) =>
      let operand = zoperand |> erase_zoperand;
      let right_zop = right_op |> place_before_operator;
      Some(ZOperator(skel, right_zop, (S(operand, prefix), sufseq)));
    | (Some(zoperand), _) => Some(ZOperand(skel, zoperand, surround))
    }
  | ZOperator(skel, zop, surround) =>
    switch (move_cursor_right_zoperator(zop), surround) {
    | (None, (preseq, sufseq)) =>
      let op = zop |> erase_zoperator;
      let (right_operand, suffix) = sufseq |> Seq.split_first_and_suffix;
      let right_zoperand = right_operand |> place_before_operand;
      Some(ZOperand(skel, right_zoperand, (A(op, preseq), suffix)));
    | (Some(zop), _) => Some(ZOperator(skel, zop, surround))
    }
  };
