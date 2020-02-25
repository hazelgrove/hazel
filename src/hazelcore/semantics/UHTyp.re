[@deriving sexp]
type operator =
  | Arrow
  | Prod
  | Sum;

let string_of_operator =
  fun
  | Arrow => UnicodeConstants.typeArrowSym
  | Prod => ","
  | Sum => "|";

let is_Prod =
  fun
  | Prod => true
  | _ => false;

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

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

let rec get_prod_elements: skel => list(skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let unwrap_parentheses = (operand: operand): t =>
  switch (operand) {
  | Hole
  | Unit
  | Num
  | Bool
  | List(_) => OpSeq.wrap(operand)
  | Parenthesized(p) => p
  };

/* TODO fix this to only parenthesize when necessary */
let contract = (ty: HTyp.t): t => {
  let mk_operand = operand => Parenthesized(OpSeq.wrap(operand));
  let mk_seq_operand = (op, a, b) => {
    let skel = Skel.BinOp(NotInHole, op, Placeholder(0), Placeholder(1));
    let seq = Seq.mk(a, [(op, b)]);
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
    | List(ty1) => List(OpSeq.wrap(ty1 |> contract_to_operand));

  ty |> contract_to_operand |> unwrap_parentheses;
};

let rec expand = (ty: t): HTyp.t => expand_opseq(ty)
and expand_opseq =
  fun
  | OpSeq(skel, seq) => expand_skel(skel, seq)
and expand_skel = (skel, seq) =>
  switch (skel) {
  | Placeholder(n) => seq |> Seq.nth_operand(n) |> expand_operand
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
