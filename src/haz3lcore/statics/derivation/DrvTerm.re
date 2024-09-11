module Jdmt = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | Val
    | Eval
    | Entail;

  include DrvTermBase.Jdmt;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Invalid => Invalid
    | Val(_) => Val
    | Eval(_) => Eval
    | Entail(_) => Entail;
};

module Prop = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | HasTy
    | Syn
    | Ana
    | Var
    | And
    | Or
    | Impl
    | Truth
    | Falsity
    | Cons
    | Nil
    | Parens;

  include DrvTermBase.Prop;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Invalid => Invalid
    | HasTy(_) => HasTy
    | Syn(_) => Syn
    | Ana(_) => Ana
    | Var(_) => Var
    | And(_) => And
    | Or(_) => Or
    | Impl(_) => Impl
    | Truth => Truth
    | Falsity => Falsity
    | Cons(_) => Cons
    | Nil => Nil
    | Parens(_) => Parens;
};

module ALFA_Exp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | NumLit
    | UnOp
    | BinOp
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
    | Parens;

  include DrvTermBase.ALFA_Exp;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Invalid => Invalid
    | NumLit(_) => NumLit
    | UnOp(_) => UnOp
    | BinOp(_) => BinOp
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
    | Parens(_) => Parens;
};

module ALFA_Pat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | Var
    | Cast
    | InjL
    | InjR
    | Ap
    | Pair
    | Parens;

  include DrvTermBase.ALFA_Pat;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Invalid => Invalid
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
    | Invalid
    | Num
    | Bool
    | Arrow
    | Prod
    | Unit
    | Sum
    | Var
    | Rec
    | Parens;

  include DrvTermBase.ALFA_Typ;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Invalid => Invalid
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
    | Invalid
    | Var;

  include DrvTermBase.ALFA_TPat;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Invalid => Invalid
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
};
