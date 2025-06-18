module TypeHole = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type cls =
    | AbbrNotVar
    | AbbrNotFound
    | AbbrNotDrvTerm
    | Invalid
    | EmptyHole
    | MultiHole;

  let show_cls =
    fun
    | AbbrNotVar
    | AbbrNotFound
    | AbbrNotDrvTerm => "Abbreviation hole"
    | Invalid => "Invalid derivation hole"
    | EmptyHole => "Empty derivation hole"
    | MultiHole => "Multiple derivation holes";

  let cls_of: DrvTermBase.type_hole => cls =
    fun
    | AbbrNotVar => AbbrNotVar
    | AbbrNotFound => AbbrNotFound
    | AbbrNotDrvTerm => AbbrNotDrvTerm
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole;
};

module Exp = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type cls =
    | Hole(TypeHole.cls)
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
    | BinOp(Grammar.Drv.op_bin)
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
    | Hole(cls) => TypeHole.show_cls(cls)
    | Quote => "Quoted variable"
    | Var => "Variable reference"
    | Parens => "Parenthesized expression"
    | Tuple => "Tuple (intermediate term)"
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

  let rep_id = ({annotation: {ids, _}, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(cls) => Hole(TypeHole.cls_of(cls))
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

  let is_hole: term => bool =
    fun
    | Hole(_) => true
    | _ => false;
};

module Pat = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type cls =
    | Hole(TypeHole.cls)
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
    | Hole(cls) => TypeHole.show_cls(cls)
    | Quote => "Quoted variable"
    | Var => "Variable pattern"
    | Cast => "Type cast pattern"
    | InjL => "Left injection pattern"
    | InjR => "Right injection pattern"
    | Ap => "Application pattern"
    | Pair => "Pair pattern"
    | Parens => "Parenthesized pattern";

  include DrvTermBase.Pat;

  let rep_id = ({annotation: {ids, _}, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(cls) => Hole(TypeHole.cls_of(cls))
    | Quote(_) => Quote
    | Var(_) => Var
    | Cast(_) => Cast
    | InjL(_) => InjL
    | InjR(_) => InjR
    | Pair(_) => Pair
    | Parens(_) => Parens;

  let is_hole: term => bool =
    fun
    | Hole(_) => true
    | _ => false;
};

module Typ = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type cls =
    | Hole(TypeHole.cls)
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
    | Hole(cls) => TypeHole.show_cls(cls)
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

  let rep_id = ({annotation: {ids, _}, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(cls) => Hole(TypeHole.cls_of(cls))
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

  let is_hole: term => bool =
    fun
    | Hole(_) => true
    | _ => false;
};

module TPat = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type cls =
    | Hole(TypeHole.cls)
    | Quote
    | Var;

  let show_cls =
    fun
    | Hole(cls) => TypeHole.show_cls(cls)
    | Var => "Variable type pattern"
    | Quote => "Quoted variable";

  include DrvTermBase.TPat;

  let rep_id = ({annotation: {ids, _}, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let cls_of_term: term => cls =
    fun
    | Hole(cls) => Hole(TypeHole.cls_of(cls))
    | Quote(_) => Quote
    | Var(_) => Var;

  let is_hole: term => bool =
    fun
    | Hole(_) => true
    | _ => false;
};

module Any = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
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

  let ids: t => list(Id.t) =
    fun
    | Exp({annotation: {ids, _}, _})
    | Pat({annotation: {ids, _}, _})
    | Typ({annotation: {ids, _}, _})
    | TPat({annotation: {ids, _}, _}) => ids;

  let cls_of: t => cls =
    fun
    | Exp(exp) => Exp(Exp.cls_of_term(exp.term))
    | Pat(pat) => Pat(Pat.cls_of_term(pat.term))
    | Typ(typ) => Typ(Typ.cls_of_term(typ.term))
    | TPat(tpat) => TPat(TPat.cls_of_term(tpat.term));

  let is_hole: t => bool =
    fun
    | Exp(exp) => Exp.is_hole(exp.term)
    | Pat(pat) => Pat.is_hole(pat.term)
    | Typ(typ) => Typ.is_hole(typ.term)
    | TPat(tpat) => TPat.is_hole(tpat.term);

  let contains_hole: t => bool =
    any => {
      exception HoleFound;
      try(
        {
          ignore(
            DrvTermBase.Any.map_term(
              ~f_exp=
                (cont, exp) =>
                  switch (exp.term) {
                  | Hole(_) => raise(HoleFound)
                  | _ => cont(exp)
                  },
              ~f_pat=
                (cont, pat) =>
                  switch (pat.term) {
                  | Hole(_) => raise(HoleFound)
                  | _ => cont(pat)
                  },
              ~f_typ=
                (cont, typ) =>
                  switch (typ.term) {
                  | Hole(_) => raise(HoleFound)
                  | _ => cont(typ)
                  },
              ~f_tpat=
                (cont, tpat) =>
                  switch (tpat.term) {
                  | Hole(_) => raise(HoleFound)
                  | _ => cont(tpat)
                  },
              ~f_any=Fun.id,
              any,
            ),
          );
          false;
        }
      ) {
      | HoleFound => true
      };
    };
};
