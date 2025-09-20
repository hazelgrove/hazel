/**
  This module describles the speculation of rules for:
  1) Unboxing
    e.g. Unboxing `let x = 1 in x` results in: `x`(pat), `1`(exp), `x`(exp)
    - An error occurs if unboxing is not possible.
  2) Unifying
    e.g. If unboxing reveals that `f` is unified with `fun x -> x`, any
    subsequent attempt to unify `f` with a value not equal to `fun x -> x`
    will result in an error.
  This module does not include `checking` functionalities, such as:
  e.g. Calculations like verifying NumLit(1) + NumLit(2) = NumLit(3) and
  subst(3, x, let y = 1 in x) = (let y = 1 in 3).
  Refer to `RuleTest.re` for checking functionalities.
 */
open DrvTermBase;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type ctx_t = list(exp_t);

module M =
       (
         W: {
           [@deriving (show({with_path: false}), sexp, yojson, eq)]
           type t('a, 'b);
         },
       ) => {
  /* [@deriving show({with_path: false})] */
  type term('a, 'b) =
    | LookUpExp(Var.t): term(exp_t, 'b)
    | LookUpPat(Var.t): term(pat_t, 'b)
    | LookUpTyp(Var.t): term(typ_t, 'b)
    | LookUpTPat(Var.t): term(tpat_t, 'b)
    | UnboxCtx(t(exp_t, 'b)): term(ctx_t, 'b)
    | UnboxNumLit(t(exp_t, 'b)): term(int, 'b)
    | UnboxExpVar(t(exp_t, 'b)): term(Var.t, 'b)
    | UnboxPatVar(t(pat_t, 'b)): term(Var.t, 'b)
    | UnboxTypVar(t(typ_t, 'b)): term(Var.t, 'b)
    | UnboxTPatVar(t(tpat_t, 'b)): term(Var.t, 'b)
    | ExpVar(t(Var.t, 'b)): term(exp_t, 'b)
    | HasType(t(exp_t, 'b), t(typ_t, 'b)): term(exp_t, 'b)
    | Type(t(typ_t, 'b)): term(exp_t, 'b)
    | Fix(t(pat_t, 'b), t(exp_t, 'b)): term(exp_t, 'b)
    | Subst(t(exp_t, 'b), t(Var.t, 'b), t(exp_t, 'b)): term(exp_t, 'b)
    | Ctx(t(ctx_t, 'b)): term(exp_t, 'b)
    | Cons(t(exp_t, 'b), t(ctx_t, 'b)): term(ctx_t, 'b)
    | Neg(t(int, 'b)): term(int, 'b)
    | Plus(t(int, 'b), t(int, 'b)): term(int, 'b)
    | Minus(t(int, 'b), t(int, 'b)): term(int, 'b)
    | Times(t(int, 'b), t(int, 'b)): term(int, 'b)
    | TypVar(t(Var.t, 'b)): term(typ_t, 'b)
    | Rec(t(tpat_t, 'b), t(typ_t, 'b)): term(typ_t, 'b)
    | Glb(t(typ_t, 'b), t(typ_t, 'b)): term(typ_t, 'b)
    | SubstTy(t(typ_t, 'b), t(Var.t, 'b), t(typ_t, 'b)): term(typ_t, 'b)
    | Ignore(t('a, 'b)): term(bool, 'b)
    | Gt(t(int, 'b), t(int, 'b)): term(bool, 'b)
    | Lt(t(int, 'b), t(int, 'b)): term(bool, 'b)
    | Eq(t(int, 'b), t(int, 'b)): term(bool, 'b)
    | NotGt(t(int, 'b), t(int, 'b)): term(bool, 'b)
    | NotLt(t(int, 'b), t(int, 'b)): term(bool, 'b)
    | NotEq(t(int, 'b), t(int, 'b)): term(bool, 'b)
    | EqExp(t(exp_t, 'b), t(exp_t, 'b)): term(bool, 'b)
    | EqCtx(t(ctx_t, 'b), t(ctx_t, 'b)): term(bool, 'b)
    | EqTyp(t(typ_t, 'b), t(typ_t, 'b)): term(bool, 'b)
    | Mem(t(exp_t, 'b), t(ctx_t, 'b)): term(bool, 'b)
    | Subset(t(ctx_t, 'b), t(ctx_t, 'b)): term(bool, 'b)
  and t('a, 'b) = W.t(term('a, 'b), 'b);

  // TODO(zhiyao): I cannot make gadt happy with deriving show,
  // sexp, and yojson.
  let t_of_sexp = (_, _, _) => failwith("not implemented");
  let sexp_of_t = (_, _, _) => failwith("not implemented");
  let t_of_yojson = (_, _, _) => failwith("cannot implemented");
  let yojson_of_t = (_, _, _) => failwith("<opaque rule-formula-t>");
  let pp = (_, _, _, _) => failwith("not implemented");

  let term_of_sexp = (_, _, _) => failwith("not implemented");
  let sexp_of_term = (_, _, _) => failwith("not implemented");
  let term_of_yojson = (_, _, _) => failwith("cannot implemented");
  let yojson_of_term = (_, _, _) => failwith("<opaque rule-formula-term>");
  let pp_term = (_, _, _, _) => failwith("not implemented");
};

module M_Annotated = M(Annotated);

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = M_Annotated.t('a, IdTagged.IdTag.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type term('a) = M_Annotated.term('a, IdTagged.IdTag.t);


let rec get_symbols: type a. t(a) => list(string) = {
  ({term, _}) => term |> get_symbols_term;
}

and get_symbols_term: type a. term(a) => list(string) =
  fun
  | LookUpExp(s) => [s]
  | LookUpPat(s) => [s]
  | LookUpTyp(s) => [s]
  | LookUpTPat(s) => [s]
  | UnboxCtx(a) => get_symbols(a)
  | UnboxNumLit(a) => get_symbols(a)
  | UnboxExpVar(a) => get_symbols(a)
  | UnboxPatVar(a) => get_symbols(a)
  | UnboxTypVar(a) => get_symbols(a)
  | UnboxTPatVar(a) => get_symbols(a)
  | ExpVar(a) => get_symbols(a)
  | HasType(a, b) => get_symbols(a) @ get_symbols(b)
  | Type(a) => get_symbols(a)
  | Fix(a, b) => get_symbols(a) @ get_symbols(b)
  | Subst(a, b, c) => get_symbols(a) @ get_symbols(b) @ get_symbols(c)
  | Ctx(a) => get_symbols(a)
  | Cons(a, b) => get_symbols(a) @ get_symbols(b)
  | Neg(a) => get_symbols(a)
  | Plus(a, b) => get_symbols(a) @ get_symbols(b)
  | Minus(a, b) => get_symbols(a) @ get_symbols(b)
  | Times(a, b) => get_symbols(a) @ get_symbols(b)
  | TypVar(a) => get_symbols(a)
  | Rec(a, b) => get_symbols(a) @ get_symbols(b)
  | Glb(a, b) => get_symbols(a) @ get_symbols(b)
  | SubstTy(a, b, c) => get_symbols(a) @ get_symbols(b) @ get_symbols(c)
  | Ignore(a) => get_symbols(a)
  | Gt(a, b) => get_symbols(a) @ get_symbols(b)
  | Lt(a, b) => get_symbols(a) @ get_symbols(b)
  | Eq(a, b) => get_symbols(a) @ get_symbols(b)
  | NotGt(a, b) => get_symbols(a) @ get_symbols(b)
  | NotLt(a, b) => get_symbols(a) @ get_symbols(b)
  | NotEq(a, b) => get_symbols(a) @ get_symbols(b)
  | EqExp(a, b) => get_symbols(a) @ get_symbols(b)
  | EqCtx(a, b) => get_symbols(a) @ get_symbols(b)
  | EqTyp(a, b) => get_symbols(a) @ get_symbols(b)
  | Mem(a, b) => get_symbols(a) @ get_symbols(b)
  | Subset(a, b) => get_symbols(a) @ get_symbols(b);

module M_Id = {
  module M_Id =
    M({
      [@deriving (show({with_path: false}), sexp, yojson, eq)]
      type t('a, 'b) = 'a;
    });
  include M_Id;


  let rec tag_term:
    type a. term(a, unit) => M_Annotated.term(a, IdTagged.IdTag.t) =
    fun
    | LookUpExp(s) => LookUpExp(s)
    | LookUpPat(s) => LookUpPat(s)
    | LookUpTyp(s) => LookUpTyp(s)
    | LookUpTPat(s) => LookUpTPat(s)
    | UnboxCtx(a) => UnboxCtx(a |> tag)
    | UnboxNumLit(a) => UnboxNumLit(a |> tag)
    | UnboxExpVar(a) => UnboxExpVar(a |> tag)
    | UnboxPatVar(a) => UnboxPatVar(a |> tag)
    | UnboxTypVar(a) => UnboxTypVar(a |> tag)
    | UnboxTPatVar(a) => UnboxTPatVar(a |> tag)
    | ExpVar(a) => ExpVar(a |> tag)
    | HasType(a, b) => HasType(a |> tag, b |> tag)
    | Type(a) => Type(a |> tag)
    | Fix(a, b) => Fix(a |> tag, b |> tag)
    | Subst(a, b, c) => Subst(a |> tag, b |> tag, c |> tag)
    | Ctx(a) => Ctx(a |> tag)
    | Cons(a, b) => Cons(a |> tag, b |> tag)
    | Neg(a) => Neg(a |> tag)
    | Plus(a, b) => Plus(a |> tag, b |> tag)
    | Minus(a, b) => Minus(a |> tag, b |> tag)
    | Times(a, b) => Times(a |> tag, b |> tag)
    | TypVar(a) => TypVar(a |> tag)
    | Rec(a, b) => Rec(a |> tag, b |> tag)
    | Glb(a, b) => Glb(a |> tag, b |> tag)
    | SubstTy(a, b, c) => SubstTy(a |> tag, b |> tag, c |> tag)
    | Ignore(a) => Ignore(a |> tag)
    | Gt(a, b) => Gt(a |> tag, b |> tag)
    | Lt(a, b) => Lt(a |> tag, b |> tag)
    | Eq(a, b) => Eq(a |> tag, b |> tag)
    | NotGt(a, b) => NotGt(a |> tag, b |> tag)
    | NotLt(a, b) => NotLt(a |> tag, b |> tag)
    | NotEq(a, b) => NotEq(a |> tag, b |> tag)
    | EqExp(a, b) => EqExp(a |> tag, b |> tag)
    | EqCtx(a, b) => EqCtx(a |> tag, b |> tag)
    | EqTyp(a, b) => EqTyp(a |> tag, b |> tag)
    | Mem(a, b) => Mem(a |> tag, b |> tag)
    | Subset(a, b) => Subset(a |> tag, b |> tag)

  and tag: type a. t(a, unit) => M_Annotated.t(a, IdTagged.IdTag.t) =
    e => e |> tag_term |> IdTagged.fresh;
};
