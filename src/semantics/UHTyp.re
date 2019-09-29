open GeneralUtil;

[@deriving sexp]
type operator =
  | Arrow
  | Prod
  | Sum;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | Hole
  | Unit
  | Num
  | Bool
  | Parenthesized(t)
  | List(t);

type skel = OpSeq.skel(operator);
type seq = OpSeq.seq(opernd, operator);
exception InconsistentOpSeq(skel, seq);

let bidelimited =
  fun
  | Hole
  | Unit
  | Num
  | Bool
  | Parenthesized(_)
  | List(_) => true;

let wrap_in_opseq = operand => OpSeq(Placeholder(0), Operand(operand));

let unwrap_parentheses = (operand: operand): opseq =>
  switch (operand) {
  | Hole
  | Unit
  | Num
  | Bool
  | List(_) => wrap_in_opseq(operand)
  | Parenthesized(opseq) => opseq
  };

/* TODO fix this to only parenthesize when necessary */
let contract = (ty: HTyp.t): t => {
  let mk_operand = operand => Parenthesized(operand |> wrap_in_opseq);
  let mk_seq_operand = (op, a, b) => {
    let skel = Skel.BinOp(NotInHole, op, Placeholder(0), Placeholder(1));
    let seq = Seq.Seq(Operand(a), op, b);
    Parenthesized(OpSeq(skel, seq));
  };
  /* Save it for another day
     match (a, b) with
       | (OpSeq skelA opseqA, OpSeq skelB opseqB) ->
       | (OpSeq skelA opseqA, _) ->
       | (_, OpSeq skelB opseqB) ->
       | (_, _) ->
         OpSeq (Skel.BinOp NotInHole op' ?? ??) (Seq.ExpOpExp a op' b)
     end
     */
  let rec contract_to_operand: HTyp.t => operand =
    fun
    | Hole => mk_operand(Hole)
    | Unit => mk_operand(Unit)
    | Num => mk_operand(Num)
    | Bool => mk_operand(Bool)
    | Arrow(ty1, ty2) =>
      mk_seq_operand(
        Arrow,
        contract_to_operand(ty1),
        contract_to_operand(ty2),
      )
    | Prod(ty1, ty2) =>
      mk_seq_operand(
        Prod,
        contract_to_operand(ty1),
        contract_to_operand(ty2),
      )
    | Sum(ty1, ty2) =>
      mk_seq_operand(
        Sum,
        contract_to_operand(ty1),
        contract_to_operand(ty2),
      )
    | List(ty1) => List(ty1 |> contract_to_operand |> wrap_in_opseq);

  ty |> contract_to_operand |> unwrap_parentheses;
};

let rec expand = (uty: t): HTyp.t => expand_opseq(uty)
and expand_opseq =
  fun
  | OpSeq(skel, seq) => expand_skel(skel, seq)
and expand_skel = (skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (Seq.nth_operand(n, seq)) {
    | None => raise(InconsistentOpSeq(skel, seq))
    | Some(uty_n) => expand_operand(uty_n)
    }
  | BinOp(_, Arrow, skel1, skel2) =>
    let ty1 = expand_skel(skel1, seq);
    let ty2 = expand_skel(skel2, seq);
    Arrow(ty1, ty2);
  | BinOp(_, Prod, skel1, skel2) =>
    let ty1 = expand_skel(skel1, seq);
    let ty2 = expand_skel(skel2, seq);
    Prod(ty1, ty2);
  | BinOp(_, Sum, skel1, skel2) =>
    let ty1 = expand_skel(skel1, seq);
    let ty2 = expand_skel(skel2, seq);
    Sum(ty1, ty2);
  }
and expand_operand =
  fun
  | Hole => Hole
  | Unit => Unit
  | Num => Num
  | Bool => Bool
  | Parenthesized(opseq) => expand(opseq)
  | List(opseq) => List(expand(opseq));

let child_indices_operand =
  fun
  | Hole
  | Unit
  | Num
  | Bool => []
  | Parenthesized(_)
  | List(_) => [0];
let child_indices_opseq =
  fun
  | OpSeq(_, seq) => seq |> Seq.length |> range;
let child_indices = child_indices_opseq;

/* TODO(dmoon)
   let favored_child: t => option((child_index, t)) =
     fun
     | Hole
     | Unit
     | Num
     | Bool
     | OpSeq(_, _) => None
     | Parenthesized(ty)
     | List(ty) => Some((0, ty));
   */
