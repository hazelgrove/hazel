open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin =
  | Plus
  | Minus
  | Times
  | Gt
  | Lt
  | Eq;

module M =
       (
         W: {
           [@deriving (show({with_path: false}), sexp, yojson, eq)]
           type t('a, 'b);
           let map: ('a => 'c, 'b => 'd, t('a, 'b)) => t('c, 'd);
         },
       ) => {
  [@deriving (show({with_path: false}), sexp, yojson, eq, map)]
  type any_t('a) =
    | Exp(exp_t('a))
    | Pat(pat_t('a))
    | Typ(typ_t('a))
    | TPat(tpat_t('a))
  and exp_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t) // Prop / Exp
    | Parens(exp_t('a)) // Jdmt / Ctxt / Prop / Exp
    | Tuple(list(exp_t('a))) // [invalid]
    // Jdmt
    | Val(exp_t('a))
    | Eval(exp_t('a), exp_t('a))
    | Entail(exp_t('a), exp_t('a))
    | Consistent(typ_t('a), typ_t('a))
    | MatchedArrow(typ_t('a), typ_t('a))
    | MatchedProd(typ_t('a), typ_t('a))
    | MatchedSum(typ_t('a), typ_t('a))
    // Ctx
    | Ctx(list(exp_t('a)))
    | Cons(exp_t('a), exp_t('a))
    | Concat(exp_t('a), exp_t('a))
    // Prop
    | Type(typ_t('a))
    | HasType(exp_t('a), typ_t('a))
    | Syn(exp_t('a), typ_t('a))
    | Ana(exp_t('a), typ_t('a))
    | And(exp_t('a), exp_t('a))
    | Or(exp_t('a), exp_t('a))
    | Impl(exp_t('a), exp_t('a))
    | Truth
    | Falsity
    // Exp
    | NumLit(int)
    | Neg(exp_t('a))
    | BinOp(op_bin, exp_t('a), exp_t('a))
    | True
    | False
    | If(exp_t('a), exp_t('a), exp_t('a))
    | Let(pat_t('a), exp_t('a), exp_t('a))
    | Fix(pat_t('a), exp_t('a))
    | Fun(pat_t('a), exp_t('a))
    | Ap(exp_t('a), exp_t('a))
    | Pair(exp_t('a), exp_t('a))
    | Triv
    | PrjL(exp_t('a))
    | PrjR(exp_t('a))
    | InjL(exp_t('a))
    | InjR(exp_t('a))
    | Case(exp_t('a), pat_t('a), exp_t('a), pat_t('a), exp_t('a))
    | Roll(exp_t('a))
    | Unroll(exp_t('a))
    | ExpHole
  and exp_t('a) = W.t(exp_term('a), 'a)
  and pat_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t)
    | Parens(pat_t('a))
    | Cast(pat_t('a), typ_t('a))
    | InjL(pat_t('a))
    | InjR(pat_t('a))
    | Pair(pat_t('a), pat_t('a))
  and pat_t('a) = W.t(pat_term('a), 'a)
  and typ_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t)
    | Parens(typ_t('a))
    | Num
    | Bool
    | Arrow(typ_t('a), typ_t('a))
    | Prod(typ_t('a), typ_t('a))
    | Unit
    | Sum(typ_t('a), typ_t('a))
    | Rec(tpat_t('a), typ_t('a))
    | TypHole
  and typ_t('a) = W.t(typ_term('a), 'a)
  and tpat_term('a) =
    | Hole(type_hole('a))
    | Quote(Var.t)
    | Var(Var.t)
  and tpat_t('a) = W.t(tpat_term('a), 'a)
  and type_hole('a) =
    | AbbrNotVar
    | AbbrNotFound
    | AbbrNotDrvTerm
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(any_t('a)));
};

module M_Annotated = M(Annotated);
include M_Annotated;
