module Jdmt = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Val
    | Eval
    | Entail;

  include TermBase.Jdmt;

  let hole = (tms: list(TermBase.Any.t)): term =>
    Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(_) => Hole
    | Val(_) => Val
    | Eval(_) => Eval
    | Entail(_) => Entail;
};

module Prop = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | HasTy
    | Syn
    | Ana
    | Var
    | And
    | Or
    | Impl
    | Truth
    | Falsity
    | Tuple
    | Abbr
    | Parens;

  include TermBase.Prop;

  let hole = (tms: list(TermBase.Any.t)): term =>
    Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(_) => Hole
    | HasTy(_) => HasTy
    | Syn(_) => Syn
    | Ana(_) => Ana
    | Var(_) => Var
    | And(_) => And
    | Or(_) => Or
    | Impl(_) => Impl
    | Truth => Truth
    | Falsity => Falsity
    | Tuple(_) => Tuple
    | Abbr(_) => Abbr
    | Parens(_) => Parens;
};

module ALFA_Exp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | NumLit
    | Neg
    | Plus
    | Minus
    | Times
    | Lt
    | Gt
    | Eq
    | True
    | False
    | If
    | Var
    | Let
    | Fix
    | Fun
    | Ap
    | Pair
    | Triv
    | PrjL
    | PrjR
    | InjL
    | InjR
    | Case
    | Roll
    | Unroll
    | Abbr
    | Parens;

  include TermBase.ALFA_Exp;

  let hole = (tms: list(TermBase.Any.t)): term =>
    Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(_) => Hole
    | NumLit(_) => NumLit
    | Neg(_) => Neg
    | Plus(_) => Plus
    | Minus(_) => Minus
    | Times(_) => Times
    | Lt(_) => Lt
    | Gt(_) => Gt
    | Eq(_) => Eq
    | True => True
    | False => False
    | If(_) => If
    | Var(_) => Var
    | Let(_) => Let
    | Fix(_) => Fix
    | Fun(_) => Fun
    | Ap(_) => Ap
    | Pair(_) => Pair
    | Triv => Triv
    | PrjL(_) => PrjL
    | PrjR(_) => PrjR
    | InjL => InjL
    | InjR => InjR
    | Case(_) => Case
    | Roll => Roll
    | Unroll => Unroll
    | Abbr(_) => Abbr
    | Parens(_) => Parens;
};

module ALFA_Pat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Var
    | Cast
    | InjL
    | InjR
    | Ap
    | Pair
    | Parens;

  include TermBase.ALFA_Pat;

  let hole = (tms: list(TermBase.Any.t)): term =>
    Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(_) => Hole
    | Var(_) => Var
    | Cast(_) => Cast
    | InjL => InjL
    | InjR => InjR
    | Ap(_) => Ap
    | Pair(_) => Pair
    | Parens(_) => Parens;
};

module ALFA_Typ = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Num
    | Bool
    | Arrow
    | Prod
    | Unit
    | Sum
    | Var
    | Rec
    | Parens;

  include TermBase.ALFA_Typ;

  let hole = (tms: list(TermBase.Any.t)): term =>
    Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(_) => Hole
    | Num => Num
    | Bool => Bool
    | Arrow(_) => Arrow
    | Prod(_) => Prod
    | Unit => Unit
    | Sum(_) => Sum
    | Var(_) => Var
    | Rec(_) => Rec
    | Parens(_) => Parens;
};

module ALFA_TPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Var;

  include TermBase.ALFA_TPat;

  let hole = (tms: list(TermBase.Any.t)): term =>
    Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(_) => Hole
    | Var(_) => Var;
};

module Drv = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Jdmt(Jdmt.cls)
    | Prop(Prop.cls)
    | Exp(ALFA_Exp.cls)
    | Pat(ALFA_Pat.cls)
    | Typ(ALFA_Typ.cls)
    | TPat(ALFA_TPat.cls);

  include TermBase.Drv;

  let of_typ: t => DrvTyp.t =
    fun
    | Jdmt(_) => Jdmt
    | Prop(_) => Prop
    | Exp(_) => Exp
    | Pat(_) => Pat
    | Typ(_) => Typ
    | TPat(_) => TPat;

  let rep_id: t => Id.t =
    fun
    | Jdmt(jdmt) => Jdmt.rep_id(jdmt)
    | Prop(prop) => Prop.rep_id(prop)
    | Exp(exp) => ALFA_Exp.rep_id(exp)
    | Pat(pat) => ALFA_Pat.rep_id(pat)
    | Typ(typ) => ALFA_Typ.rep_id(typ)
    | TPat(tpat) => ALFA_TPat.rep_id(tpat);

  let of_id: t => list(Id.t) =
    fun
    | Jdmt(jdmt) => jdmt.ids
    | Prop(prop) => prop.ids
    | Exp(exp) => exp.ids
    | Pat(pat) => pat.ids
    | Typ(typ) => typ.ids
    | TPat(tpat) => tpat.ids;

  let cls_of: t => cls =
    fun
    | Jdmt(jdmt) => Jdmt(Jdmt.cls_of_term(jdmt.term))
    | Prop(prop) => Prop(Prop.cls_of_term(prop.term))
    | Exp(exp) => Exp(ALFA_Exp.cls_of_term(exp.term))
    | Pat(pat) => Pat(ALFA_Pat.cls_of_term(pat.term))
    | Typ(typ) => Typ(ALFA_Typ.cls_of_term(typ.term))
    | TPat(tpat) => TPat(ALFA_TPat.cls_of_term(tpat.term));
};
