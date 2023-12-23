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
  | Syn
  | Ana(Typ.t);

let ana: Typ.t => t = ty => Ana(ty);

/* The expected type imposed by a mode */
let ty_of = (ctx: Ctx.t, t: t, id: Id.t): (Typ.t, Typ.constraints) =>
  switch (t) {
  | Ana(ty) => (ty, [])
  | Syn => (Typ.unknown_synswitch(id), [])
  | SynFun =>
    let ((ty_l, ty_r), constraints) =
      Typ.matched_arrow(ctx, id, Typ.unknown_synswitch(id));
    (Arrow(ty_l, ty_r), constraints);
  };

let assistant_ty_of: t => Typ.t =
  fun
  | Ana(ty) => ty
  | Syn => Unknown(NoProvenance, false)
  | SynFun =>
    Arrow(Unknown(NoProvenance, false), Unknown(NoProvenance, false));

let of_arrow =
    (ctx: Ctx.t, mode: t, termId: Id.t): ((t, t), Typ.constraints) =>
  switch (mode) {
  | Syn
  | SynFun => ((Syn, Syn), [])
  | Ana(ty) =>
    let (matched_typs, constraints) = Typ.matched_arrow(ctx, termId, ty);
    (TupleUtil.map2(ana, matched_typs), constraints);
  };

let of_prod =
    (ctx: Ctx.t, mode: t, termId: Id.t, length): (list(t), Typ.constraints) =>
  switch (mode) {
  | Syn
  | SynFun => (List.init(length, _ => Syn), [])
  | Ana(ty) =>
    let (tys, constraints) = Typ.matched_prod(ctx, length, termId, ty);
    (List.map(ana, tys), constraints);
  };

let of_cons_hd = (ctx: Ctx.t, mode: t, termId: Id.t): (t, Typ.constraints) =>
  switch (mode) {
  | Syn
  | SynFun => (Syn, [])
  | Ana(ty) =>
    let (matched_ty, constraints) = Typ.matched_list(ctx, termId, ty);
    (Ana(matched_ty), constraints);
  };

let of_cons_tl =
    (ctx: Ctx.t, mode: t, hd_ty: Typ.t, termId: Id.t): (t, Typ.constraints) =>
  switch (mode) {
  | Syn
  | SynFun => (Ana(List(hd_ty)), [])
  | Ana(ty) =>
    let (matched_ty, constraints) = Typ.matched_list(ctx, termId, ty);
    (Ana(List(matched_ty)), constraints);
  };

let of_list = (ctx: Ctx.t, mode: t, termId: Id.t): (t, Typ.constraints) =>
  switch (mode) {
  | Syn
  | SynFun => (Syn, [])
  | Ana(ty) =>
    let (matched_typ, constraints) = Typ.matched_list(ctx, termId, ty);
    (Ana(matched_typ), constraints);
  };

let of_list_concat = (ctx: Ctx.t, id, mode: t): (t, Typ.constraints) =>
  switch (mode) {
  | Syn
  | SynFun => (Ana(List(Typ.unknown_synswitch(id))), [])
  | Ana(ty) =>
    let (matched_typ, constraints) = Typ.matched_list(ctx, id, ty);
    (Ana(List(matched_typ)), constraints);
  };

let of_list_lit =
    (ctx: Ctx.t, length, termId: Id.t, mode: t): (list(t), Typ.constraints) => {
  let (typs, constraint_lists) =
    List.init(length, _ => of_list(ctx, mode, termId)) |> List.split;
  let constraints = List.flatten(constraint_lists);
  (typs, constraints);
};

// TODO: anand and raef; discuss if the mode ctr fns below need constraints

let ctr_ana_typ = (ctx: Ctx.t, mode: t, ctr: Constructor.t): option(Typ.t) => {
  /* If a ctr is being analyzed against (an arrow type returning)
     a sum type having that ctr as a variant, we consider the
     ctr's type to be determined by the sum type */
  switch (mode) {
  | Ana(Arrow(_, ty_ana))
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
    Some(Ana(Arrow(Unknown(NoProvenance, false), ty_ana)))
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
