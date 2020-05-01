// TODO
// type t('operand, 'operator) = (Skel.t('operator), Seq.t('operand, 'operator))
[@deriving sexp]
type t('operand, 'operator) =
  | OpSeq(skel('operator), seq('operand, 'operator))
and skel('operator) = Skel.t('operator)
and seq('operand, 'operator) = Seq.t('operand, 'operator);

let mk =
    (
      ~associate: seq('operand, 'operator) => Skel.t('operator),
      seq: seq('operand, 'operator),
    )
    : t('operand, 'operator) =>
  OpSeq(seq |> associate, seq);

let wrap = (operand: 'operand): t('operand, _) =>
  OpSeq(Placeholder(0), S(operand, E));

let get_err_status =
    (
      ~get_err_status_operand: 'operand => ErrStatus.t,
      opseq: t('operand, 'operator),
    )
    : ErrStatus.t =>
  switch (opseq) {
  | OpSeq(BinOp(err, _, _, _), _) => err
  | OpSeq(Placeholder(n), seq) =>
    Seq.nth_operand(n, seq) |> get_err_status_operand
  };

let set_err_status =
    (
      ~set_err_status_operand: (ErrStatus.t, 'operand) => 'operand,
      err: ErrStatus.t,
      opseq: t('operand, 'operator),
    )
    : t('operand, 'operator) =>
  switch (opseq) {
  | OpSeq(BinOp(_, op, skel1, skel2), seq) =>
    OpSeq(BinOp(err, op, skel1, skel2), seq)
  | OpSeq(Placeholder(n) as skel, seq) =>
    let set_operand =
      seq |> Seq.nth_operand(n) |> set_err_status_operand(err);
    let set_seq = seq |> Seq.update_nth_operand(n, set_operand);
    OpSeq(skel, set_seq);
  };

let make_inconsistent =
    (
      ~make_inconsistent_operand:
         (MetaVarGen.t, 'operand) => ('operand, MetaVarGen.t),
      u_gen: MetaVarGen.t,
      opseq: t('operand, 'operator),
    )
    : (t('operand, 'operator), MetaVarGen.t) =>
  switch (opseq) {
  | OpSeq(Placeholder(n) as skel, seq) =>
    let (set_operand, u_gen) =
      seq |> Seq.nth_operand(n) |> make_inconsistent_operand(u_gen);
    let set_seq = seq |> Seq.update_nth_operand(n, set_operand);
    (OpSeq(skel, set_seq), u_gen);
  | OpSeq(BinOp(InHole(TypeInconsistent, _), _, _, _), _) => (opseq, u_gen)
  | OpSeq(
      BinOp(NotInHole, op, skel1, skel2) |
      BinOp(InHole(WrongLength, _), op, skel1, skel2),
      seq,
    ) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let set_skel = Skel.BinOp(InHole(TypeInconsistent, u), op, skel1, skel2);
    (OpSeq(set_skel, seq), u_gen);
  };
