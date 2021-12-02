open Sexplib.Std;

[@deriving sexp]
type operator = Operators_Typ.t;

[@deriving sexp]
type sum_body_operator = Operators_SumBody.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | FiniteSum(option(sum_body))
  | ElidedSum(sum_body_operand)
  | Parenthesized(t)
  | List(t)
and sum_body = OpSeq.t(sum_body_operand, sum_body_operator)
and sum_body_operand =
  | ConstTag(UHTag.t)
  | ArgTag(UHTag.t, t);

/* sum { C + ?(?) } */

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

[@deriving sexp]
type sum_body_skel = OpSeq.skel(sum_body_operator);
[@deriving sexp]
type sum_body_seq = OpSeq.seq(sum_body_operand, sum_body_operator);

let rec get_prod_elements: skel => list(skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let rec get_sum_body_elements: sum_body_skel => list(sum_body_skel) =
  fun
  | BinOp(_, Plus, skel1, skel2) =>
    get_sum_body_elements(skel1) @ get_sum_body_elements(skel2)
  | skel => [skel];

let unwrap_parentheses = (operand: operand): t =>
  switch (operand) {
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | FiniteSum(_)
  | ElidedSum(_)
  | List(_) => OpSeq.wrap(operand)
  | Parenthesized(p) => p
  };

let associate =
  Skel.mk(Operators_Typ.precedence, Operators_Typ.associativity);

let associate_sum_body =
  Skel.mk(Operators_SumBody.precedence, Operators_SumBody.associativity);

let mk_OpSeq = OpSeq.mk(~associate);

let mk_OpSeq_sum_body = OpSeq.mk(~associate=associate_sum_body);

let rec contract = (ty: HTyp.t): t => {
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
      switch (ty) {
      | Hole => Seq.wrap(Hole)
      | Int => Seq.wrap(Int)
      | Float => Seq.wrap(Float)
      | Bool => Seq.wrap(Bool)
      | Arrow(ty1, ty2) =>
        mk_seq_operand(HTyp.precedence_Arrow, Operators_Typ.Arrow, ty1, ty2)
      | Prod([]) => Seq.wrap(Unit)
      | Prod([head, ...tail]) =>
        tail
        |> List.map(elementType =>
             contract_to_seq(
               ~parenthesize=
                 HTyp.precedence(elementType) > HTyp.precedence_Prod,
               elementType,
             )
           )
        |> List.fold_left(
             (seq1, seq2) => Seq.seq_op_seq(seq1, Operators_Typ.Prod, seq2),
             contract_to_seq(
               ~parenthesize=HTyp.precedence(head) > HTyp.precedence_Prod,
               head,
             ),
           )
      | Sum(Finite(tymap)) =>
        switch (TagMap.bindings(tymap)) {
        | [] => Seq.wrap(FiniteSum(None))
        | [head, ...tail] =>
          let sum_body_bindings =
            tail
            |> List.map(seq_of_binding)
            |> List.fold_left(
                 (seq1, seq2) =>
                   Seq.seq_op_seq(seq1, Operators_SumBody.Plus, seq2),
                 seq_of_binding(head),
               );
          Seq.wrap(FiniteSum(Some(mk_OpSeq_sum_body(sum_body_bindings))));
        }
      | Sum(Elided(tag, ty_opt)) =>
        let operand =
          switch (ty_opt) {
          | None => ConstTag(tag)
          | Some(ty) => ArgTag(tag, contract(ty))
          };
        Seq.wrap(ElidedSum(operand));
      | List(ty1) =>
        Seq.wrap(List(ty1 |> contract_to_seq |> OpSeq.mk(~associate)))
      };
    if (parenthesize) {
      Seq.wrap(Parenthesized(OpSeq.mk(~associate, seq)));
    } else {
      seq;
    };
  }
  and seq_of_binding = ((tag1, ty1_opt)) =>
    switch (ty1_opt) {
    | None => Seq.wrap(ConstTag(tag1))
    | Some(ty1) => Seq.wrap(ArgTag(tag1, mk_OpSeq(contract_to_seq(ty1))))
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
    Arrow(ty1, ty2);
  | BinOp(_, Prod, _, _) =>
    Prod(
      skel |> get_prod_elements |> List.map(skel => expand_skel(skel, seq)),
    )
  }
and expand_operand =
  fun
  | Hole => Hole
  | Unit => Prod([])
  | Int => Int
  | Float => Float
  | Bool => Bool
  | FiniteSum(None) => Sum(Finite(TagMap.empty))
  | FiniteSum(Some(opseq)) => Sum(Finite(expand_sum_body(opseq)))
  | ElidedSum(operand) => {
      let (tag, ty_opt) = expand_sum_body_operand(operand);
      Sum(Elided(tag, ty_opt));
    }
  | Parenthesized(opseq) => expand(opseq)
  | List(opseq) => List(expand(opseq))
and expand_sum_body = (OpSeq(_skel, seq)) =>
  Seq.operands(seq) |> List.map(expand_sum_body_operand) |> TagMap.of_list
and expand_sum_body_operand =
  fun
  | ConstTag(tag) => (tag, None)
  | ArgTag(tag, opseq) => (tag, Some(expand(opseq)));

let rec is_complete = (ty: t) => {
  switch (ty) {
  | OpSeq(sk, sq) => is_complete_skel(sk, sq)
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
and is_complete_operand = (operand: 'operand) => {
  switch (operand) {
  | Hole => false
  | Unit => true
  | Int => true
  | Float => true
  | Bool => true
  | FiniteSum(None) => true
  | FiniteSum(Some(sum_body)) => is_complete_sum_body(sum_body)
  | ElidedSum(operand) => is_complete_sum_body_operand(operand)
  | Parenthesized(body) => is_complete(body)
  | List(body) => is_complete(body)
  };
}
and is_complete_sum_body =
  fun
  | OpSeq(skel, seq) => is_complete_sum_body_skel(skel, seq)
and is_complete_sum_body_skel = (sk: sum_body_skel, sq: sum_body_seq) => {
  switch (sk) {
  | Placeholder(n) as _skel =>
    is_complete_sum_body_operand(sq |> Seq.nth_operand(n))
  | BinOp(InHole(_), _, _, _) => false
  | BinOp(NotInHole, _, skel1, skel2) =>
    is_complete_sum_body_skel(skel1, sq)
    && is_complete_sum_body_skel(skel2, sq)
  };
}
and is_complete_sum_body_operand = (operand: sum_body_operand) => {
  switch (operand) {
  | ConstTag(EmptyTagHole(_))
  | ArgTag(EmptyTagHole(_), _) => false
  | ConstTag(Tag(_)) => true
  | ArgTag(Tag(_), ty) => is_complete(ty)
  };
};

let duplicate_tags = (OpSeq(_, seq): sum_body): UHTag.Set.t => {
  let rec histogram = (xs: list('a)): UHTag.Map.t(int) => {
    let incr_opt =
      fun
      | None => Some(1)
      | Some(n) => Some(n + 1);
    switch (xs) {
    | [] => UHTag.Map.empty
    | [hd, ...tl] => histogram(tl) |> UHTag.Map.update(hd, incr_opt)
    };
  };
  Seq.operands(seq)
  |> List.map(
       fun
       | ConstTag(Tag(_, t)) => UHTag.Tag(NotInTagHole, t)
       | ConstTag(EmptyTagHole(_) as tag) => tag
       | ArgTag(Tag(_, t), _) => UHTag.Tag(NotInTagHole, t)
       | ArgTag(EmptyTagHole(_) as tag, _) => tag,
     )
  |> histogram
  |> UHTag.Map.bindings
  |> List.filter(((_, n)) => n > 1)
  |> List.map(fst)
  |> UHTag.Set.of_list;
};

let rec fix_holes =
        (OpSeq(skel, seq): t, u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (skel, seq, u_gen) = fix_holes_skel(skel, seq, u_gen);
  (OpSeq(skel, seq), u_gen);
}

and fix_holes_skel =
    (skel: skel, seq: seq, u_gen: MetaVarGen.t): (skel, seq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    let operand = seq |> Seq.nth_operand(n);
    let (operand, u_gen) = fix_holes_operand(operand, u_gen);
    let seq = seq |> Seq.update_nth_operand(n, operand);
    (skel, seq, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    let (skel1, seq, u_gen) = fix_holes_skel(skel1, seq, u_gen);
    let (skel2, seq, u_gen) = fix_holes_skel(skel2, seq, u_gen);
    (BinOp(NotInHole, op, skel1, skel2), seq, u_gen);
  }

and fix_holes_operand =
    (operand: operand, u_gen: MetaVarGen.t): (operand, MetaVarGen.t) =>
  switch (operand) {
  | Hole
  | Unit
  | Int
  | Float
  | Bool
  | FiniteSum(None) => (operand, u_gen)
  | FiniteSum(Some(sum_body)) =>
    let (sum_body, u_gen) = fix_holes_sum_body(sum_body, u_gen);
    (FiniteSum(Some(sum_body)), u_gen);
  | ElidedSum(operand') =>
    let (operand'', u_gen) =
      fix_holes_sum_body_operand(operand', UHTag.Set.empty, u_gen);
    (ElidedSum(operand''), u_gen);
  | Parenthesized(body) =>
    let (body, u_gen) = fix_holes(body, u_gen);
    (Parenthesized(body), u_gen);
  | List(body) =>
    let (body, u_gen) = fix_holes(body, u_gen);
    (List(body), u_gen);
  }

and fix_holes_sum_body =
    (OpSeq(skel, seq) as sum_body: sum_body, u_gen: MetaVarGen.t)
    : (sum_body, MetaVarGen.t) => {
  let dups = duplicate_tags(sum_body);
  let (skel, seq, u_gen) = fix_holes_sum_body_skel(skel, seq, dups, u_gen);
  (OpSeq(skel, seq), u_gen);
}

and fix_holes_sum_body_skel =
    (
      skel: sum_body_skel,
      seq: sum_body_seq,
      dups: UHTag.Set.t,
      u_gen: MetaVarGen.t,
    )
    : (sum_body_skel, sum_body_seq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    let operand = seq |> Seq.nth_operand(n);
    let (operand, u_gen) = fix_holes_sum_body_operand(operand, dups, u_gen);
    let seq = seq |> Seq.update_nth_operand(n, operand);
    (skel, seq, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    let (skel1, seq, u_gen) =
      fix_holes_sum_body_skel(skel1, seq, dups, u_gen);
    let (skel2, seq, u_gen) =
      fix_holes_sum_body_skel(skel2, seq, dups, u_gen);
    (BinOp(NotInHole, op, skel1, skel2), seq, u_gen);
  }

and fix_holes_sum_body_operand =
    (operand: sum_body_operand, dups: UHTag.Set.t, u_gen: MetaVarGen.t)
    : (sum_body_operand, MetaVarGen.t) =>
  switch (operand) {
  | ConstTag(tag) =>
    let (tag, u_gen) = UHTag.fix_holes(tag, dups, u_gen);
    (ConstTag(tag), u_gen);
  | ArgTag(tag, ty) =>
    let (tag, u_gen) = UHTag.fix_holes(tag, dups, u_gen);
    let (ty, u_gen) = fix_holes(ty, u_gen);
    (ArgTag(tag, ty), u_gen);
  };
