module Exp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Var
    | Abbr
    | Parens
    | Tuple
    | Val
    | Eval
    | Entail
    | Consistent
    | MatchedArrow
    | MatchedProd
    | MatchedSum
    | Ctx
    | Cons
    | Concat
    | Type
    | HasType
    | Syn
    | Ana
    | And
    | Or
    | Impl
    | Truth
    | Falsity
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
    | Let
    | Fix
    | Fun
    | Ap
    | Triv
    | PrjL
    | PrjR
    | InjL
    | InjR
    | Case
    | Roll
    | Unroll
    | ExpHole;

  include DrvTermBase.Exp;

  // let hole = (tms: list(TermBase.Any.t)): term =>
  //   Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

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
    | Abbr(_) => Abbr
    | Parens(_) => Parens
    | Val(_) => Val
    | Eval(_) => Eval
    | Entail(_) => Entail
    | Consistent(_) => Consistent
    | MatchedArrow(_) => MatchedArrow
    | MatchedProd(_) => MatchedProd
    | MatchedSum(_) => MatchedSum
    | Ctx(_) => Ctx
    | Cons(_) => Cons
    | Concat(_) => Concat
    | Type(_) => Type
    | HasType(_) => HasType
    | Syn(_) => Syn
    | Ana(_) => Ana
    | And(_) => And
    | Or(_) => Or
    | Impl(_) => Impl
    | Truth => Truth
    | Falsity => Falsity
    | Tuple(_) => Tuple
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
    | Let(_) => Let
    | Fix(_) => Fix
    | Fun(_) => Fun
    | Ap(_) => Ap
    | Triv => Triv
    | PrjL(_) => PrjL
    | PrjR(_) => PrjR
    | InjL => InjL
    | InjR => InjR
    | Case(_) => Case
    | Roll => Roll
    | Unroll => Unroll
    | ExpHole => ExpHole;
};

module Rul = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Rules;

  include DrvTermBase.Rul;

  // let hole = (tms: list(TermBase.Any.t)): term =>
  //   Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

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
    | Rules(_, _) => Rules;
};

module Pat = {
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

  include DrvTermBase.Pat;

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

module Typ = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Abbr
    | Num
    | Bool
    | Arrow
    | Prod
    | Unit
    | Sum
    | Var
    | Rec
    | Parens
    | TypHole;

  include DrvTermBase.Typ;

  // let hole = (tms: list(TermBase.Any.t)): term =>
  //   Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

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
    | Abbr(_) => Abbr
    | Num => Num
    | Bool => Bool
    | Arrow(_) => Arrow
    | Prod(_) => Prod
    | Unit => Unit
    | Sum(_) => Sum
    | Var(_) => Var
    | Rec(_) => Rec
    | Parens(_) => Parens
    | TypHole => TypHole;
};

module TPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Var;

  include DrvTermBase.TPat;

  // let hole = (tms: list(TermBase.Any.t)): term =>
  //   Hole(List.is_empty(tms) ? EmptyHole : MultiHole(tms));

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

module Any = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Exp(Exp.cls)
    | Rul(Rul.cls)
    | Pat(Pat.cls)
    | Typ(Typ.cls)
    | TPat(TPat.cls);

  include DrvTermBase.Any;

  let sort_of: t => DrvSort.t =
    fun
    | Exp(_) => Exp
    | Rul(_) => Rul
    | Pat(_) => Pat
    | Typ(_) => Typ
    | TPat(_) => TPat
    | Any(_) => Any;

  let rep_id: t => Id.t =
    fun
    | Exp(exp) => Exp.rep_id(exp)
    | Rul(rul) => Rul.rep_id(rul)
    | Pat(pat) => Pat.rep_id(pat)
    | Typ(typ) => Typ.rep_id(typ)
    | TPat(tpat) => TPat.rep_id(tpat)
    | Any(_) => raise(Invalid_argument("Any.rep_id"));

  let of_id: t => list(Id.t) =
    fun
    | Exp(exp) => exp.ids
    | Rul(rul) => rul.ids
    | Pat(pat) => pat.ids
    | Typ(typ) => typ.ids
    | TPat(tpat) => tpat.ids
    | Any(_) => [];

  let cls_of: t => cls =
    fun
    | Exp(exp) => Exp(Exp.cls_of_term(exp.term))
    | Rul(rul) => Rul(Rul.cls_of_term(rul.term))
    | Pat(pat) => Pat(Pat.cls_of_term(pat.term))
    | Typ(typ) => Typ(Typ.cls_of_term(typ.term))
    | TPat(tpat) => TPat(TPat.cls_of_term(tpat.term))
    | Any(_) => raise(Invalid_argument("Any.cls_of"));
};
