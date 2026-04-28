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
    | BinOp(DrvGrammar.op_bin)
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
    | Quote => "quoted variable"
    | Var => "variable reference"
    | Parens => "parenthesized expression"
    | Tuple => "tuple (intermediate term)"
    | Val => "value judgement"
    | Eval => "evaluation judgement"
    | Entail => "entailment judgement"
    | Consistent => "type consistency judgement"
    | MatchedArrow => "type matching arrow judgement"
    | MatchedProd => "type matching product judgement"
    | MatchedSum => "type matching sum judgement"
    | Ctx => "proposition context"
    | Cons => "context cons"
    | Concat => "context concatenation"
    | Type => "type validation proposition"
    | HasType => "expression has type proposition"
    | Syn => "expression synthesis type proposition"
    | Ana => "expression analysis type proposition"
    | And => "conjunction proposition"
    | Or => "disjunction proposition"
    | Impl => "implication proposition"
    | Truth => "tautology proposition"
    | Falsity => "absurdity proposition"
    | NumLit => "number literal"
    | Neg => "negation expression"
    | BinOp(Plus) => "addition expression"
    | BinOp(Minus) => "subtraction expression"
    | BinOp(Times) => "multiplication expression"
    | BinOp(Lt) => "less than expression"
    | BinOp(Gt) => "greater than expression"
    | BinOp(Eq) => "equality expression"
    | True => "boolean literal true"
    | False => "boolean literal false"
    | If => "if expression"
    | Let => "let expression"
    | Fix => "fixpoint operator"
    | Fun => "function literal"
    | Ap => "application"
    | Pair => "pair expression"
    | Triv => "unit literal"
    | PrjL => "left projection expression"
    | PrjR => "right projection expression"
    | InjL => "left injection expression"
    | InjR => "right injection expression"
    | Case => "pattern matching expression"
    | Roll => "roll expression"
    | Unroll => "unroll expression"
    | ExpHole => "expression hole";

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
    | Quote => "quoted variable"
    | Var => "variable pattern"
    | Cast => "type cast pattern"
    | InjL => "left injection pattern"
    | InjR => "right injection pattern"
    | Ap => "application pattern"
    | Pair => "pair pattern"
    | Parens => "parenthesized pattern";

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
    | Quote => "quoted variable"
    | Var => "variable type"
    | Parens => "parenthesized type"
    | Num => "number type"
    | Bool => "boolean type"
    | Arrow => "arrow type"
    | Prod => "product type"
    | Unit => "unit type"
    | Sum => "sum type"
    | Rec => "recursive type"
    | TypHole => "type hole";

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
