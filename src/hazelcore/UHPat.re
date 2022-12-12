open Sexplib.Std;

exception FreeVarInPat;

[@deriving sexp]
type operator = Operators_Pat.t;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | Wild(ErrStatus.t)
  | TypeAnn(ErrStatus.t, operand, UHTyp.t)
  | InvalidText(MetaVar.t, string)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | IntLit(ErrStatus.t, string)
  | FloatLit(ErrStatus.t, string)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Parenthesized(t)
  | Inj(ErrStatus.t, InjSide.t, t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      x: Var.t,
    )
    : operand =>
  Var(err, var_err, x);

let wild = (~err: ErrStatus.t=NotInHole, ()) => Wild(err);

let boollit = (~err: ErrStatus.t=NotInHole, b: bool) => BoolLit(err, b);

let intlit = (~err: ErrStatus.t=NotInHole, n: string) => IntLit(err, n);

let floatlit = (~err: ErrStatus.t=NotInHole, f: string) => FloatLit(err, f);

let listnil = (~err: ErrStatus.t=NotInHole, ()) => ListNil(err);

let rec get_tuple_elements: skel => list(skel) =
  fun
  | BinOp(_, Comma, skel1, skel2) =>
    get_tuple_elements(skel1) @ get_tuple_elements(skel2)
  | skel => [skel];

let rec mk_tuple = (~err: ErrStatus.t=NotInHole, elements: list(skel)): skel =>
  switch (elements) {
  | [] => failwith("mk_tuple: expected at least 1 element")
  | [skel] => skel
  | [skel, ...skels] => BinOp(err, Comma, skel, mk_tuple(skels))
  };

let new_InvalidText = (id_gen: IDGen.t, t: string): (operand, IDGen.t) => {
  let (u, id_gen) = IDGen.next_hole(id_gen);
  (InvalidText(u, t), id_gen);
} /* helper function for constructing a new empty hole */;

let new_EmptyHole = (id_gen: IDGen.t): (operand, IDGen.t) => {
  let (u, id_gen) = IDGen.next_hole(id_gen);
  (EmptyHole(u), id_gen);
};

let is_EmptyHole =
  fun
  | EmptyHole(_) => true
  | _ => false;

let rec get_err_status = (p: t) => get_err_status_opseq(p)
and get_err_status_opseq = opseq =>
  OpSeq.get_err_status(~get_err_status_operand, opseq)
and get_err_status_operand =
  fun
  | EmptyHole(_) => NotInHole
  | InvalidText(_, _) => NotInHole
  | Wild(err)
  | Var(err, _, _)
  | IntLit(err, _)
  | FloatLit(err, _)
  | BoolLit(err, _)
  | ListNil(err)
  | TypeAnn(err, _, _)
  | Inj(err, _, _) => err
  | Parenthesized(p) => get_err_status(p);

let rec set_err_status = (err: ErrStatus.t, p: t): t =>
  p |> set_err_status_opseq(err)
and set_err_status_opseq = (err, opseq) =>
  OpSeq.set_err_status(~set_err_status_operand, err, opseq)
and set_err_status_operand = (err, operand) =>
  switch (operand) {
  | EmptyHole(_) => operand
  | InvalidText(_, _) => operand
  | Wild(_) => Wild(err)
  | Var(_, var_err, x) => Var(err, var_err, x)
  | IntLit(_, n) => IntLit(err, n)
  | FloatLit(_, f) => FloatLit(err, f)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  | Inj(_, inj_side, p) => Inj(err, inj_side, p)
  | Parenthesized(p) => Parenthesized(set_err_status(err, p))
  | TypeAnn(_, op, ann) => TypeAnn(err, op, ann)
  } /* put p in a new hole, if it is not already in a hole */;

let rec mk_inconsistent = (id_gen: IDGen.t, p: t): (t, IDGen.t) =>
  mk_inconsistent_opseq(id_gen, p)
and mk_inconsistent_opseq = (id_gen: IDGen.t, opseq: opseq): (opseq, IDGen.t) =>
  opseq |> OpSeq.mk_inconsistent(~mk_inconsistent_operand, id_gen)
and mk_inconsistent_operand =
    (id_gen: IDGen.t, operand: operand): (operand, IDGen.t) =>
  switch (operand) {
  // already in hole
  | EmptyHole(_)
  | InvalidText(_, _)
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _) => (operand, id_gen)
  | TypeAnn(InHole(TypeInconsistent, _), _, _) => (operand, id_gen)
  // not in hole
  | Wild(NotInHole | InHole(WrongLength, _))
  | Var(NotInHole | InHole(WrongLength, _), _, _)
  | IntLit(NotInHole | InHole(WrongLength, _), _)
  | FloatLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Inj(NotInHole | InHole(WrongLength, _), _, _)
  | TypeAnn(NotInHole | InHole(WrongLength, _), _, _) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    let set_operand =
      operand |> set_err_status_operand(InHole(TypeInconsistent, u));
    (set_operand, id_gen);
  | Parenthesized(p) =>
    let (set_p, id_gen) = p |> mk_inconsistent(id_gen);
    (Parenthesized(set_p), id_gen);
  };

let text_operand = (id_gen: IDGen.t, shape: TextShape.t): (operand, IDGen.t) =>
  switch (shape) {
  | Underscore => (wild(), id_gen)
  | IntLit(n) => (intlit(n), id_gen)
  | FloatLit(n) => (floatlit(n), id_gen)
  | BoolLit(b) => (boollit(b), id_gen)
  | Var(x) => (var(x), id_gen)
  | ExpandingKeyword(kw) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    (
      var(~var_err=InVarHole(Free, u), kw |> ExpandingKeyword.to_string),
      id_gen,
    );
  | InvalidTextShape(t) => new_InvalidText(id_gen, t)
  };

let associate =
  Skel.mk(Operators_Pat.precedence, Operators_Pat.associativity);

let mk_OpSeq = OpSeq.mk(~associate);

let rec is_complete_skel = (sk: skel, sq: seq): bool => {
  switch (sk) {
  | Placeholder(n) as _skel => is_complete_operand(sq |> Seq.nth_operand(n))
  | BinOp(InHole(_), _, _, _) => false
  | BinOp(NotInHole, _, skel1, skel2) =>
    is_complete_skel(skel1, sq) && is_complete_skel(skel2, sq)
  };
}
and is_complete = (p: t): bool => {
  switch (p) {
  | OpSeq(sk, sq) => is_complete_skel(sk, sq)
  };
}
and is_complete_operand = (operand: 'operand): bool => {
  switch (operand) {
  | EmptyHole(_) => false
  | InvalidText(_, _) => false
  | Wild(InHole(_)) => false
  | Wild(NotInHole) => true
  | Var(InHole(_), _, _) => false
  | Var(NotInHole, InVarHole(_), _) => false
  | Var(NotInHole, NotInVarHole, _) => true
  | IntLit(InHole(_), _) => false
  | IntLit(NotInHole, _) => true
  | FloatLit(InHole(_), _) => false
  | FloatLit(NotInHole, _) => true
  | BoolLit(InHole(_), _) => false
  | BoolLit(NotInHole, _) => true
  | ListNil(InHole(_)) => false
  | ListNil(NotInHole) => true
  | Parenthesized(body) => is_complete(body)
  | TypeAnn(_, op, ann) => is_complete_operand(op) && UHTyp.is_complete(ann)
  | Inj(InHole(_), _, _) => false
  | Inj(NotInHole, _, body) => is_complete(body)
  };
};
