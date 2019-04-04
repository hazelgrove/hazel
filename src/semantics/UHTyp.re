[@deriving sexp]
type op =
  | Arrow
  | Prod
  | Sum;

[@deriving sexp]
type skel_t = Skel.t(op);

[@deriving sexp]
type t =
  | TO(t_outer)
  | TI(t_inner)
and t_outer =
  | Hole
  | Unit
  | Num
  | Bool
and t_inner =
  | Parenthesized(t)
  | List(t)
  | OpSeq(skel_t, opseq)
and opseq = OperatorSeq.opseq(t, op);

let bidelimited = (uty: t): bool =>
  switch (uty) {
  | TO(_) => true
  | TI(Parenthesized(_)) => true
  | TI(List(_)) => true
  | TI(OpSeq(_, _)) => false
  };

let rec well_formed = (uty: t): bool =>
  switch (uty) {
  | TO(Hole) => true
  | TO(Unit) => true
  | TO(Num) => true
  | TO(Bool) => true
  | TI(Parenthesized(uty1)) => well_formed(uty1)
  | TI(List(uty1)) => well_formed(uty1)
  | TI(OpSeq(skel, seq)) =>
    /* NOTE: does not check that skel is the valid parse of seq */
    well_formed_skel(skel, seq)
  }
and well_formed_skel = (skel: skel_t, seq: opseq): bool =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | Some(uty_n) => bidelimited(uty_n) && well_formed(uty_n)
    | None => false
    }
  | BinOp(NotInHole, _, skel1, skel2) =>
    well_formed_skel(skel1, seq) && well_formed_skel(skel2, seq)
  | BinOp(InHole(TypeInconsistent, _), _, _, _) => false /* no type-level non-empty holes */
  | BinOp(InHole(WrongLength, _), _, _, _) => false
  }; /* the type is assumed to be the true length */

/* TODO fix this to only parenthesize when necessary */
let rec contract = (ty: HTyp.t): t => {
  let mk_opseq = (op', a, b) => {
    let ph = n => Skel.Placeholder(n);
    let skel = Skel.BinOp(NotInHole, op', ph(0), ph(1));
    TI(Parenthesized(TI(OpSeq(skel, ExpOpExp(a, op', b)))));
  };
  /* Save it for another day
     match (a, b) with
       | (OpSeq skelA opseqA, OpSeq skelB opseqB) ->
       | (OpSeq skelA opseqA, _) ->
       | (_, OpSeq skelB opseqB) ->
       | (_, _) ->
         OpSeq (Skel.BinOp NotInHole op' ?? ??) (OperatorSeq.ExpOpExp a op' b)
     end
     */

  switch (ty) {
  | Hole => TO(Hole)
  | Unit => TO(Unit)
  | Num => TO(Num)
  | Bool => TO(Bool)
  | Arrow(ty1, ty2) => mk_opseq(Arrow, contract(ty1), contract(ty2))
  | Prod(ty1, ty2) => mk_opseq(Prod, contract(ty1), contract(ty2))
  | Sum(ty1, ty2) => mk_opseq(Sum, contract(ty1), contract(ty2))
  | List(ty1) => TI(List(contract(ty1)))
  };
};

let rec expand = (uty: t): HTyp.t =>
  switch (uty) {
  | TO(Hole) => Hole
  | TO(Unit) => Unit
  | TO(Num) => Num
  | TO(Bool) => Bool
  | TI(Parenthesized(uty1)) => expand(uty1)
  | TI(List(uty1)) => List(expand(uty1))
  | TI(OpSeq(skel, seq)) => expand_skel(skel, seq)
  }
and expand_skel = (skel: skel_t, seq: opseq): HTyp.t =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | Some(uty_n) => expand(uty_n)
    | None => Hole /* should never happen */
    }
  | BinOp(_, Arrow, skel1, skel2) =>
    let uty1 = expand_skel(skel1, seq);
    let uty2 = expand_skel(skel2, seq);
    Arrow(uty1, uty2);
  | BinOp(_, Prod, skel1, skel2) =>
    let uty1 = expand_skel(skel1, seq);
    let uty2 = expand_skel(skel2, seq);
    Prod(uty1, uty2);
  | BinOp(_, Sum, skel1, skel2) =>
    let uty1 = expand_skel(skel1, seq);
    let uty2 = expand_skel(skel2, seq);
    Sum(uty1, uty2);
  };
