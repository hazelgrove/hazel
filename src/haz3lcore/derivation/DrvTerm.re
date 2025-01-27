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

  let show_cls =
    fun
    | Hole => "Expression hole"
    | Var => "Variable reference"
    | Abbr => "Abbreviation"
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
    | Plus => "Addition expression"
    | Minus => "Subtraction expression"
    | Times => "Multiplication expression"
    | Lt => "Less than expression"
    | Gt => "Greater than expression"
    | Eq => "Equality expression"
    | True => "Boolean literal true"
    | False => "Boolean literal false"
    | If => "If expression"
    | Let => "Let expression"
    | Fix => "Fixpoint operator"
    | Fun => "Function literal"
    | Ap => "Application"
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
    | InjL(_) => InjL
    | InjR(_) => InjR
    | Case(_) => Case
    | Roll(_) => Roll
    | Unroll(_) => Unroll
    | ExpHole => ExpHole;
};

module Rul = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    | Rules;

  let show_cls =
    fun
    | Hole => "Rule hole"
    | Rules => "Rules";

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

  let show_cls =
    fun
    | Hole => "Pattern hole"
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

  let show_cls =
    fun
    | Hole => "Type hole"
    | Abbr => "Type abbreviation"
    | Num => "Number type"
    | Bool => "Boolean type"
    | Arrow => "Arrow type"
    | Prod => "Product type"
    | Unit => "Unit type"
    | Sum => "Sum type"
    | Var => "Type variable"
    | Rec => "Recursive type"
    | Parens => "Parenthesized type"
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

  let show_cls =
    fun
    | Hole => "Type pattern hole"
    | Var => "Type pattern variable";

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

  let show_cls =
    fun
    | Exp(cls) => Exp.show_cls(cls)
    | Rul(cls) => Rul.show_cls(cls)
    | Pat(cls) => Pat.show_cls(cls)
    | Typ(cls) => Typ.show_cls(cls)
    | TPat(cls) => TPat.show_cls(cls);

  include DrvTermBase.Any;

  let rep_id: t => Id.t =
    fun
    | Exp(exp) => Exp.rep_id(exp)
    | Rul(rul) => Rul.rep_id(rul)
    | Pat(pat) => Pat.rep_id(pat)
    | Typ(typ) => Typ.rep_id(typ)
    | TPat(tpat) => TPat.rep_id(tpat);

  let of_id: t => list(Id.t) =
    fun
    | Exp(exp) => exp.ids
    | Rul(rul) => rul.ids
    | Pat(pat) => pat.ids
    | Typ(typ) => typ.ids
    | TPat(tpat) => tpat.ids;

  let cls_of: t => cls =
    fun
    | Exp(exp) => Exp(Exp.cls_of_term(exp.term))
    | Rul(rul) => Rul(Rul.cls_of_term(rul.term))
    | Pat(pat) => Pat(Pat.cls_of_term(pat.term))
    | Typ(typ) => Typ(Typ.cls_of_term(typ.term))
    | TPat(tpat) => TPat(TPat.cls_of_term(tpat.term));
};
