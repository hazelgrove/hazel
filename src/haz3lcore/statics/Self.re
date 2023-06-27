//open Util.OptUtil.Syntax;
open Sexplib.Std;

/* The common (synthetic) type information derivable from pattern
   or expression terms in isolation, using the typing context but
   not the syntactic context i.e. typing mode */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Just(Typ.t) /* Just a regular type */
  | NoJoin(list(Typ.source)) /* Inconsistent types for e.g match, listlits */
  | BadToken(Token.t) /* Invalid expression token, treated as hole */
  | IsMulti /* Multihole, treated as hole */
  | IsTag({
      name: Token.t,
      syn_ty: option(Typ.t),
    }); /* Tags have special ana logic */

/* The self for expressions could also be a free variable */
[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | FreeVar
  | Common(t);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  | Common(t);

/* What the type would be if the position had been
   synthetic, so no hole fixing. Returns none if
   there's no applicable synthetic rule. */
let typ_of: (Ctx.t, t) => option(Typ.t) =
  _ctx =>
    fun
    | Just(typ) => Some(typ)
    | IsTag({syn_ty, _}) => syn_ty
    | BadToken(_)
    | IsMulti
    | NoJoin(_) => None;

let typ_of_exp: (Ctx.t, exp) => option(Typ.t) =
  ctx =>
    fun
    | FreeVar => None
    | Common(self) => typ_of(ctx, self);

/* The self of a var depends on the ctx; if the
   lookup fails, it is a free variable */
let of_exp_var = (ctx: Ctx.t, name: Token.t): exp =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => FreeVar
  | Some(var) => Common(Just(var.typ))
  };

/* The self of a tag depends on the ctx, but a
   lookup failure doesn't necessarily means its
   free; it may be given a type analytically */
let of_tag = (ctx: Ctx.t, name: Token.t): t =>
  IsTag({
    name,
    syn_ty:
      switch (Ctx.lookup_tag(ctx, name)) {
      | None => None
      | Some({typ, _}) => Some(typ)
      },
  });

/* The self assigned to things like cases and list literals
   which can have internal type inconsistencies. */
let join =
    (wrap: Typ.t => Typ.t, tys: list(Typ.t), ids: list(Id.t), ctx: Ctx.t): t =>
  switch (Typ.join_all(ctx, tys)) {
  | None => NoJoin(List.map2((id, ty) => Typ.{id, ty}, ids, tys))
  | Some(ty) => Just(wrap(ty))
  };
