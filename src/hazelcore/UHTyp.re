open Sexplib.Std;

[@deriving sexp]
type operator = Operators_Typ.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | TyVar(TyVarErrStatus.t, string)
  | Hole
  | Unit
  | Int
  | Float
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
  | TyVar(_)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) => OpSeq.wrap(operand)
  | Parenthesized(p) => p
  };

let associate =
  Skel.mk(Operators_Typ.precedence, Operators_Typ.associativity);

let mk_OpSeq = OpSeq.mk(~associate);

let contract = (ty: HTyp.t): t => {
  let rec mk_seq_operand = (precedence_op, op, ty1, ty2) =>
    Seq.seq_op_seq(
      contract_to_seq(
        ~parenthesize=HTyp.precedence(ty1) > precedence_op,
        ty1,
      ),
      op,
      contract_to_seq(
        ~parenthesize=HTyp.precedence(ty2) >= precedence_op,
        ty2,
      ),
    )
  and contract_to_seq = (~parenthesize=false, ty: HTyp.t) => {
    let seq =
      switch (HTyp.to_syntax(ty)) {
      | Hole => Seq.wrap(Hole)
      | TyVar(idx, t) => Seq.wrap(TyVar(NotInTyVarHole(idx), t))
      | TyVarHole(reason, u, t) => Seq.wrap(TyVar(InHole(reason, u), t))
      | Int => Seq.wrap(Int)
      | Float => Seq.wrap(Float)
      | Bool => Seq.wrap(Bool)
      | Arrow(ty1, ty2) =>
        mk_seq_operand(
          HTyp.precedence_Arrow(),
          Operators_Typ.Arrow,
          HTyp.of_syntax(ty1),
          HTyp.of_syntax(ty2),
        )
      | Prod([]) => Seq.wrap(Unit)
      | Prod([head, ...tail]) =>
        tail
        |> List.map(elementType =>
             contract_to_seq(
               ~parenthesize=
                 HTyp.precedence(HTyp.of_syntax(elementType))
                 > HTyp.precedence_Prod(),
               HTyp.of_syntax(elementType),
             )
           )
        |> List.fold_left(
             (seq1, seq2) => Seq.seq_op_seq(seq1, Operators_Typ.Prod, seq2),
             contract_to_seq(
               ~parenthesize=
                 HTyp.precedence(HTyp.of_syntax(head))
                 > HTyp.precedence_Prod(),
               HTyp.of_syntax(head),
             ),
           )
      | Sum(ty1, ty2) =>
        mk_seq_operand(
          HTyp.precedence_Sum(),
          Sum,
          HTyp.of_syntax(ty1),
          HTyp.of_syntax(ty2),
        )
      | List(ty1) =>
        Seq.wrap(
          List(
            HTyp.of_syntax(ty1) |> contract_to_seq |> OpSeq.mk(~associate),
          ),
        )
      };
    if (parenthesize) {
      Seq.wrap(Parenthesized(OpSeq.mk(~associate, seq)));
    } else {
      seq;
    };
  };
  ty |> contract_to_seq |> OpSeq.mk(~associate);
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
    HTyp.arrow(ty1, ty2);
  | BinOp(_, Prod, _, _) =>
    HTyp.product(
      skel |> get_prod_elements |> List.map(skel => expand_skel(skel, seq)),
    )
  | BinOp(_, Sum, skel1, skel2) =>
    let ty1 = expand_skel(skel1, seq);
    let ty2 = expand_skel(skel2, seq);
    HTyp.sum(ty1, ty2);
  }
and expand_operand =
  fun
  | TyVar(NotInTyVarHole(idx), t) => HTyp.tyvar(idx, t)
  | TyVar(InHole(reason, u), t) => HTyp.tyvarhole(reason, u, t)
  | Hole => HTyp.hole()
  | Unit => HTyp.product([])
  | Int => HTyp.int()
  | Float => HTyp.float()
  | Bool => HTyp.bool()
  | Parenthesized(opseq) => expand(opseq)
  | List(opseq) => HTyp.list(expand(opseq));

let rec is_complete_operand = (operand: 'operand) => {
  switch (operand) {
  | Hole => false
  | TyVar(NotInTyVarHole(_), _) => true
  | TyVar(InHole(_), _) => false
  | Unit => true
  | Int => true
  | Float => true
  | Bool => true
  | Parenthesized(body) => is_complete(body)
  | List(body) => is_complete(body)
  };
}
and is_complete_skel = (sk: skel, sq: seq) => {
  switch (sk) {
  | Placeholder(n) as _skel => is_complete_operand(sq |> Seq.nth_operand(n))
  | BinOp(InHole(_), _, _, _) => false
  | BinOp(NotInHole, _, skel1, skel2) =>
    is_complete_skel(skel1, sq) && is_complete_skel(skel2, sq)
  };
}
and is_complete = (ty: t) => {
  switch (ty) {
  | OpSeq(sk, sq) => is_complete_skel(sk, sq)
  };
};
