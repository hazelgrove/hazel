open Util;
open OptUtil.Syntax;

/* MODE.re

     This module defines the (analytic) type expectation imposed by a term's
      syntactic context, in particular its immediate parent. The most common
      cases are either Syn (no type expectation), or Ana (some type expectation).

      A term's MODE is used in combination with that term's SELF (Self.re) by
      to determine that term's STATUS (Info.re), which dictates whether or not
      it is placed in a hole, and hence its FIXED TYPE (Info.re).

      (It is conjectured [citation needed] that the Syn mode is functionally
      indistinguishable from Ana(Unknown(SynSwitch)), and that this type is
      thus vestigial.)

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SynFun /* Used only in function position of applications */
  | SynInfix
  | Syn
  | Ana(Typ.t)
  | AnaInfix(Typ.t);

let ana: Typ.t => t = ty => Ana(ty);

/* The expected type imposed by a mode */
let ty_of: t => Typ.t =
  fun
  | AnaInfix(ty)
  | Ana(ty) => ty
  | Syn => Unknown(SynSwitch)
  | SynInfix
  | SynFun => Arrow(Unknown(SynSwitch), Unknown(SynSwitch));

let of_arrow = (ctx: Ctx.t, mode: t): (t, t) =>
  switch (mode) {
  | Syn
  | SynInfix
  | SynFun => (Syn, Syn)
  | AnaInfix(ty)
  | Ana(ty) => ty |> Typ.matched_arrow(ctx) |> TupleUtil.map2(ana)
  };

let of_prod = (ctx: Ctx.t, mode: t, length): list(t) =>
  switch (mode) {
  | Syn
  | SynInfix
  | SynFun => List.init(length, _ => Syn)
  | AnaInfix(ty)
  | Ana(ty) => ty |> Typ.matched_prod(ctx, length) |> List.map(ana)
  };

let of_cons_hd = (ctx: Ctx.t, mode: t): t =>
  switch (mode) {
  | Syn
  | SynInfix
  | SynFun => Syn
  | AnaInfix(ty)
  | Ana(ty) => Ana(Typ.matched_list(ctx, ty))
  };

let of_cons_tl = (ctx: Ctx.t, mode: t, hd_ty: Typ.t): t =>
  switch (mode) {
  | Syn
  | SynInfix
  | SynFun => Ana(List(hd_ty))
  | AnaInfix(ty)
  | Ana(ty) => Ana(List(Typ.matched_list(ctx, ty)))
  };

let of_list = (ctx: Ctx.t, mode: t): t =>
  switch (mode) {
  | Syn
  | SynInfix
  | SynFun => Syn
  | AnaInfix(ty)
  | Ana(ty) => Ana(Typ.matched_list(ctx, ty))
  };

let of_list_concat = (ctx: Ctx.t, mode: t): t =>
  switch (mode) {
  | Syn
  | SynInfix
  | SynFun => Ana(List(Unknown(SynSwitch)))
  | AnaInfix(ty)
  | Ana(ty) => Ana(List(Typ.matched_list(ctx, ty)))
  };

let of_list_lit = (ctx: Ctx.t, length, mode: t): list(t) =>
  List.init(length, _ => of_list(ctx, mode));

let ctr_ana_typ = (ctx: Ctx.t, mode: t, ctr: Constructor.t): option(Typ.t) => {
  /* If a ctr is being analyzed against (an arrow type returning)
     a sum type having that ctr as a variant, we consider the
     ctr's type to be determined by the sum type */
  switch (mode) {
  | AnaInfix(Arrow(_, ty_ana))
  | Ana(Arrow(_, ty_ana))
  | AnaInfix(ty_ana)
  | Ana(ty_ana) =>
    let* ctrs = Typ.get_sum_constructors(ctx, ty_ana);
    let+ (_, ty_entry) = Typ.sum_entry(ctr, ctrs);
    switch (ty_entry) {
    | None => ty_ana
    | Some(ty_in) => Arrow(ty_in, ty_ana)
    };
  | _ => None
  };
};

let of_ctr_in_ap = (ctx: Ctx.t, mode: t, ctr: Constructor.t): option(t) =>
  switch (ctr_ana_typ(ctx, mode, ctr)) {
  | Some(Arrow(_) as ty_ana) => Some(Ana(ty_ana))
  | Some(ty_ana) =>
    /* Consider for example "let _ : +Yo = Yo("lol") in..."
       Here, the 'Yo' constructor should be in a hole, as it
       is nullary but used as unary; we reflect this by analyzing
       against an arrow type. Since we can't guess at what the
       parameter type might have be, we use Unknown. */
    Some(Ana(Arrow(Unknown(Internal), ty_ana)))
  | None => None
  };

let of_ap = (ctx, mode, ctr: option(Constructor.t)): t =>
  /* If a ctr application is being analyzed against a sum type for
     which that ctr is a variant, then we consider the ctr to be in
     analytic mode against an arrow returning that sum type; otherwise
     we use the typical mode for function applications */
  switch (ctr) {
  | Some(name) =>
    switch (of_ctr_in_ap(ctx, mode, name)) {
    | Some(mode) => mode
    | _ => SynFun
    }
  | None => SynFun
  };

let of_op = (ctx: Ctx.t, op: TermBase.UExp.t): (t, t) => {
  /* Extracts the type of a operator name and wraps it ina Mode.t. If
     there is no variable found, return SynInfix. */
  let op_name =
    switch (op) {
    | {term: Var(name), _} => name
    | _ => failwith("Non-Var passed to UserOp")
    };
  switch (Ctx.lookup_var(ctx, op_name)) {
  | Some(var) =>
    let (ty_in, _) = Typ.matched_arrow(ctx, var.typ);
    (AnaInfix(var.typ), Ana(ty_in));
  | None => (SynInfix, Syn)
  };
};

let of_deferred_ap_args = (length: int, ty_ins: list(Typ.t)): list(t) =>
  (
    List.length(ty_ins) == length
      ? ty_ins : List.init(length, _ => Typ.Unknown(Internal))
  )
  |> List.map(ty => Ana(ty));
