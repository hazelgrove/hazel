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

/* The expected type imposed by a mode */
let ty_of: t => Typ.t =
  fun
  | Ana(ty) => ty
  | Syn => Unknown(SynSwitch)
  | SynFun => Arrow(Unknown(SynSwitch), Unknown(SynSwitch));

let of_arrow: t => (t, t) =
  fun
  | Syn
  | SynFun => (Syn, Syn)
  | Ana(ty) => {
      let (ty_in, ty_out) = Typ.matched_arrow(ty);
      (Ana(ty_in), Ana(ty_out));
    };

let of_prod = (mode: t, length): list(t) =>
  switch (mode) {
  | Syn
  | SynFun => List.init(length, _ => Syn)
  | Ana(ty) => Typ.matched_prod(length, ty) |> List.map(ty => Ana(ty))
  };

let of_cons_hd: t => t =
  fun
  | Syn
  | SynFun => Syn
  | Ana(ty) => Ana(Typ.matched_list(ty));

let of_cons_tl = (mode: t, hd_ty: Typ.t): t =>
  switch (mode) {
  | Syn
  | SynFun => Ana(List(hd_ty))
  | Ana(ty) => Ana(List(Typ.matched_list(ty)))
  };

let of_list: t => t =
  fun
  | Syn
  | SynFun => Syn
  | Ana(ty) => Ana(Typ.matched_list(ty));

let of_list_lit = (length, mode: t): list(t) =>
  List.init(length, _ => of_list(mode));

let tag_ana_typ = (ctx: Ctx.t, mode: t, tag: Tag.t): option(Typ.t) => {
  /* If a tag is being analyzed against (an arrow type returning)
     a sum type having that tag as a variant, we consider the
     tag's type to be determined by the sum type */
  switch (mode) {
  | Ana(Arrow(_, ty_ana))
  | Ana(ty_ana) =>
    let* tags = Typ.get_sum_tags(ctx, ty_ana);
    let+ (_, ty_entry) = Typ.sum_entry(tag, tags);
    switch (ty_entry) {
    | None => ty_ana
    | Some(ty_in) => Arrow(ty_in, ty_ana)
    };
  | _ => None
  };
};

let of_tag_in_ap = (ctx: Ctx.t, mode: t, tag: Tag.t): option(t) =>
  switch (tag_ana_typ(ctx, mode, tag)) {
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

let of_ap = (ctx, mode, tag: option(Tag.t)): t =>
  /* If a tag application is being analyzed against a sum type for
     which that tag is a variant, then we consider the tag to be in
     analytic mode against an arrow returning that sum type; otherwise
     we use the typical mode for function applications */
  switch (tag) {
  | Some(name) =>
    switch (of_tag_in_ap(ctx, mode, name)) {
    | Some(mode) => mode
    | _ => SynFun
    }
  | None => SynFun
  };
