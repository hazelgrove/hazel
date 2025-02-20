module Exp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Var
    | Quote
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
    | BinOp(DrvTermBase.op_bin)
    | True
    | False
    | If
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
    | ExpHole;

  let show_cls =
    fun
    | Hole => "Expression hole"
    | Quote => "Quoted variable"
    | Var => "Variable reference"
    | Parens => "Parenthesized expression"
    | Tuple => "Pair literal"
    | Val => "Value judgement"
    | Eval => "Evaluation judgement"
    | Entail => "Entailment judgement"
    | Consistent => "Type consistency judgement"
    | MatchedArrow => "Type matching arrow judgement"
    | MatchedProd => "Type matching product judgement"
    | MatchedSum => "Type matching sum judgement"
    | Ctx => "Proposition context"
    | Cons => "Context Cons"
    | Concat => "Context Concatenation"
    | Type => "Type validation proposition"
    | HasType => "Expression has type proposition"
    | Syn => "Expression synthesis type proposition"
    | Ana => "Expression analysis type proposition"
    | And => "Conjunction proposition"
    | Or => "Disjunction proposition"
    | Impl => "Implication proposition"
    | Truth => "Tautology proposition"
    | Falsity => "Absurdity proposition"
    | NumLit => "Number literal"
    | Neg => "Negation expression"
    | BinOp(Plus) => "Addition expression"
    | BinOp(Minus) => "Subtraction expression"
    | BinOp(Times) => "Multiplication expression"
    | BinOp(Lt) => "Less than expression"
    | BinOp(Gt) => "Greater than expression"
    | BinOp(Eq) => "Equality expression"
    | True => "Boolean literal true"
    | False => "Boolean literal false"
    | If => "If expression"
    | Let => "Let expression"
    | Fix => "Fixpoint operator"
    | Fun => "Function literal"
    | Ap => "Application"
    | Pair => "Pair expression"
    | Triv => "Unit literal"
    | PrjL => "Left projection expression"
    | PrjR => "Right projection expression"
    | InjL => "Left injection expression"
    | InjR => "Right injection expression"
    | Case => "Pattern matching expression"
    | Roll => "Roll expression"
    | Unroll => "Unroll expression"
    | ExpHole => "Expression hole";

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
    | Quote(_) => Quote
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
    | BinOp(cls, _, _) => BinOp(cls)
    | True => True
    | False => False
    | If(_) => If
    | Let(_) => Let
    | Fix(_) => Fix
    | Fun(_) => Fun
    | Ap(_) => Ap
    | Pair(_) => Pair
    | Triv => Triv
    | PrjL(_) => PrjL
    | PrjR(_) => PrjR
    | InjL(_) => InjL
    | InjR(_) => InjR
    | Case(_) => Case
    | Roll(_) => Roll
    | Unroll(_) => Unroll
    | ExpHole => ExpHole;
};

module Pat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Quote
    | Var
    | Cast
    | InjL
    | InjR
    | Ap
    | Pair
    | Parens;

  let show_cls =
    fun
    | Hole => "Pattern hole"
    | Quote => "Quoted variable"
    | Var => "Variable pattern"
    | Cast => "Type cast pattern"
    | InjL => "Left injection pattern"
    | InjR => "Right injection pattern"
    | Ap => "Application pattern"
    | Pair => "Pair pattern"
    | Parens => "Parenthesized pattern";

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
    | Quote(_) => Quote
    | Var(_) => Var
    | Cast(_) => Cast
    | InjL(_) => InjL
    | InjR(_) => InjR
    | Pair(_) => Pair
    | Parens(_) => Parens;
};

module Typ = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Quote
    | Var
    | Parens
    | Num
    | Bool
    | Arrow
    | Prod
    | Unit
    | Sum
    | Rec
    | TypHole;

  let show_cls =
    fun
    | Hole => "Type hole"
    | Quote => "Quoted variable"
    | Var => "Variable type"
    | Parens => "Parenthesized type"
    | Num => "Number type"
    | Bool => "Boolean type"
    | Arrow => "Arrow type"
    | Prod => "Product type"
    | Unit => "Unit type"
    | Sum => "Sum type"
    | Rec => "Recursive type"
    | TypHole => "Type hole";

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
    | Quote(_) => Quote
    | Var(_) => Var
    | Parens(_) => Parens
    | Num => Num
    | Bool => Bool
    | Arrow(_) => Arrow
    | Prod(_) => Prod
    | Unit => Unit
    | Sum(_) => Sum
    | Rec(_) => Rec
    | TypHole => TypHole;
};

module TPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Quote
    | Var;

  let show_cls =
    fun
    | Hole => "Type pattern hole"
    | Var => "Variable type pattern"
    | Quote => "Quoted variable";

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
    | Quote(_) => Quote
    | Var(_) => Var;
};

module Any = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Exp(Exp.cls)
    | Pat(Pat.cls)
    | Typ(Typ.cls)
    | TPat(TPat.cls);

  let show_cls =
    fun
    | Exp(cls) => Exp.show_cls(cls)
    | Pat(cls) => Pat.show_cls(cls)
    | Typ(cls) => Typ.show_cls(cls)
    | TPat(cls) => TPat.show_cls(cls);

  include DrvTermBase.Any;

  let rep_id: t => Id.t =
    fun
    | Exp(exp) => Exp.rep_id(exp)
    | Pat(pat) => Pat.rep_id(pat)
    | Typ(typ) => Typ.rep_id(typ)
    | TPat(tpat) => TPat.rep_id(tpat);

  let of_id: t => list(Id.t) =
    fun
    | Exp(exp) => exp.ids
    | Pat(pat) => pat.ids
    | Typ(typ) => typ.ids
    | TPat(tpat) => tpat.ids;

  let cls_of: t => cls =
    fun
    | Exp(exp) => Exp(Exp.cls_of_term(exp.term))
    | Pat(pat) => Pat(Pat.cls_of_term(pat.term))
    | Typ(typ) => Typ(Typ.cls_of_term(typ.term))
    | TPat(tpat) => TPat(TPat.cls_of_term(tpat.term));
};
