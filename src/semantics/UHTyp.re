[@deriving sexp]
type op =
  | Arrow
  | Prod
  | Sum;

[@deriving sexp]
type skel_t = Skel.t(op);

[@deriving sexp]
type t =
  | Hole
  | Unit
  | Num
  | Bool
  | Parenthesized(t)
  | List(t)
  | OpSeq(skel_t, OperatorSeq.opseq(t, op));

type opseq = OperatorSeq.opseq(t, op);

let bidelimited =
  fun
  | Hole
  | Unit
  | Num
  | Bool
  | Parenthesized(_) => true
  | List(_) => true
  | OpSeq(_, _) => false;

let rec well_formed = uty =>
  switch (uty) {
  | Hole => true
  | Unit => true
  | Num => true
  | Bool => true
  | Parenthesized(uty1) => well_formed(uty1)
  | List(uty1) => well_formed(uty1)
  | OpSeq(skel, seq) =>
    /* NOTE: does not check that skel is the valid parse of seq */
    well_formed_skel(skel, seq)
  }
and well_formed_skel = (skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | Some(uty_n) => bidelimited(uty_n) && well_formed(uty_n)
    | None => false
    }
  | Skel.BinOp(NotInHole, _, skel1, skel2) =>
    well_formed_skel(skel1, seq) && well_formed_skel(skel2, seq)
  | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _) => false /* no type-level non-empty holes */
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => false
  }; /* the type is assumed to be the true length */

/* TODO fix this to only parenthesize when necessary */
let rec contract = ty => {
  let mk_opseq = (op', a, b) => {
    let ph = n => Skel.Placeholder(n);
    let skel = Skel.BinOp(NotInHole, op', ph(0), ph(1));
    Parenthesized(OpSeq(skel, OperatorSeq.ExpOpExp(a, op', b)));
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
  | HTyp.Hole => Hole
  | HTyp.Unit => Unit
  | HTyp.Num => Num
  | HTyp.Bool => Bool
  | HTyp.Arrow(ty1, ty2) => mk_opseq(Arrow, contract(ty1), contract(ty2))
  | HTyp.Prod(ty1, ty2) => mk_opseq(Prod, contract(ty1), contract(ty2))
  | HTyp.Sum(ty1, ty2) => mk_opseq(Sum, contract(ty1), contract(ty2))
  | HTyp.List(ty1) => List(contract(ty1))
  };
};

let rec expand = uty =>
  switch (uty) {
  | Hole => HTyp.Hole
  | Unit => HTyp.Unit
  | Num => HTyp.Num
  | Bool => HTyp.Bool
  | Parenthesized(uty1) => expand(uty1)
  | List(uty1) => HTyp.List(expand(uty1))
  | OpSeq(skel, seq) => expand_skel(skel, seq)
  }
and expand_skel = (skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | Some(uty_n) => expand(uty_n)
    | None => HTyp.Hole /* should never happen */
    }
  | Skel.BinOp(_, Arrow, skel1, skel2) =>
    let uty1 = expand_skel(skel1, seq);
    let uty2 = expand_skel(skel2, seq);
    HTyp.Arrow(uty1, uty2);
  | Skel.BinOp(_, Prod, skel1, skel2) =>
    let uty1 = expand_skel(skel1, seq);
    let uty2 = expand_skel(skel2, seq);
    HTyp.Prod(uty1, uty2);
  | Skel.BinOp(_, Sum, skel1, skel2) =>
    let uty1 = expand_skel(skel1, seq);
    let uty2 = expand_skel(skel2, seq);
    HTyp.Sum(uty1, uty2);
  };
