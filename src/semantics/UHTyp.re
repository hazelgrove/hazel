open GeneralUtil;
open SemanticsCommon;

[@deriving sexp]
type op =
  | Arrow
  | Prod
  | Sum;

[@deriving (sexp, show)]
type skel_t = Skel.t(op);

[@deriving (sexp, show)]
type t =
  | TVar(var_err_status, Var.t) /* bound type variables */
  | Hole
  | Unit
  | Num
  | Bool
  | Parenthesized(t)
  | List(t)
  | OpSeq(skel_t, OperatorSeq.opseq(t, op))
  | Forall(TPat.t, t);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

/* See UHTyp.re for an explanation of bidelimited. */
let bidelimited =
  fun
  | Hole
  | Unit
  | Num
  | Bool
  | TVar(_)
  | Parenthesized(_)
  | List(_) => true
  | OpSeq(_, _) => false
  | Forall(_, _) => false;

/* let rec well_formed = uty => */
/*   switch (uty) { */
/*   | Hole => true */
/*   | Unit => true */
/*   | Num => */
/*     true; */
/*     interested; */
/*   | Bool => true */
/*   | Parenthesized(uty1) => well_formed(uty1) */
/*   | List(uty1) => well_formed(uty1) */
/*   | OpSeq(skel, seq) => */
/*     /1* NOTE: does not check that skel is the valid parse of seq *1/ */
/*     well_formed_skel(skel, seq) */
/*   } */
/* and well_formed_skel = (skel, seq) => */
/*   switch (skel) { */
/*   | Skel.Placeholder(n) => */
/*     switch (OperatorSeq.seq_nth(n, seq)) { */
/*     | Some(uty_n) => bidelimited(uty_n) && well_formed(uty_n) */
/*     | None => false */
/*     } */
/*   | Skel.BinOp(NotInHole, _, skel1, skel2) => */
/*     well_formed_skel(skel1, seq) && well_formed_skel(skel2, seq) */
/*   | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _) => false /1* no type-level non-empty holes *1/ */
/*   | Skel.BinOp(InHole(WrongLength, _), _, _, _) => false */
/*   }; /1* the type is assumed to be the true length *1/ */

/* TODO fix this to only parenthesize when necessary */
let rec contract = (ty: HTyp.t): t => {
  let mk_opseq = (op', a, b) => {
    let ph = n => Skel.Placeholder(n);
    let skel = Skel.BinOp(NotInHole, op', ph(0), ph(1));
    Parenthesized(OpSeq(skel, ExpOpExp(a, op', b)));
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
  | HTyp.TVar(_, t) => TVar(NotInVHole, t)
  | HTyp.TVarHole(u, t) => TVar(InVHole(Free, u), t)
  | HTyp.Hole => Hole
  | HTyp.Unit => Unit
  | HTyp.Num => Num
  | HTyp.Bool => Bool
  | HTyp.Arrow(ty1, ty2) => mk_opseq(Arrow, contract(ty1), contract(ty2))
  | HTyp.Prod(ty1, ty2) => mk_opseq(Prod, contract(ty1), contract(ty2))
  | HTyp.Sum(ty1, ty2) => mk_opseq(Sum, contract(ty1), contract(ty2))
  | HTyp.List(ty1) => List(contract(ty1))
  | HTyp.Forall(t, ty) => Forall(t, contract(ty))
  };
};

let rec expand = (uty: t): HTyp.t =>
  switch (uty) {
  /*! fix this */
  | TVar(NotInVHole, t) => HTyp.TVar(0, t)
  | TVar(InVHole(_, u), ty) => HTyp.TVarHole(u, ty)
  | Hole => HTyp.Hole
  | Unit => HTyp.Unit
  | Num => HTyp.Num
  | Bool => HTyp.Bool
  | Parenthesized(uty1) => expand(uty1)
  | List(uty1) => List(expand(uty1))
  | OpSeq(skel, seq) => expand_skel(skel, seq)
  | Forall(tpat, ty) => HTyp.Forall(tpat, expand(ty))
  }
and expand_skel = (skel: skel_t, seq: opseq): HTyp.t =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
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

let child_indices =
  fun
  | Hole
  | Unit
  | Num
  | Bool => []
  | Parenthesized(_)
  | List(_) => [0]
  | OpSeq(_, seq) => range(OperatorSeq.seq_length(seq));

let favored_child: t => option((child_index, t)) =
  fun
  | Hole
  | Unit
  | Num
  | Bool
  | OpSeq(_, _) => None
  | Parenthesized(ty)
  | List(ty) => Some((0, ty));

let new_ForallHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (Forall(TPat.Hole(u), Hole), u_gen);
};
