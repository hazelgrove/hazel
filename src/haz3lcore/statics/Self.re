open Util;
open Sexplib.Std;

/* SELF.re

   This module defines the SELF data structure, which represents
   the synthetic type information derivable from a term independent
   of the type expectation (i.e. MODE) of its syntactic context. This
   synethetic information is not entirely independent, in that it still
   uses the typing context passed down from the syntactic context.

   A term which from which a type can be derived in isolation, that is,
   that has a valid synthetic typing judgement, will generally have a SELF
   of Just(some_type). (The one current exception are the tags of labelled
   sum types, which are handled specially as their synthetic type
   may be 'overwritten' by the analytic expectation)

   The other cases all represent states for which no single type can be
   derived, such as syntactic errors, or branching constructs which may
   have inconsistent types.

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Just(Typ.t) /* Just a regular type */
  | NoJoin(list(Typ.source)) /* Inconsistent types for e.g match, listlits */
  | BadToken(Token.t) /* Invalid expression token, treated as hole */
  | IsMulti /* Multihole, treated as hole */
  | IsTag({
      name: Tag.t,
      syn_ty: option(Typ.t),
    }); /* Tags have special ana logic */

/* Expressions can also be free variables */
[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | FreeVar
  | FreeDeferral
  | IsMeaninglessPartialAp
  // | IsInconsistentPartialApArg(Typ.t, list(option(Typ.t))) /* Inconsistent partial application argument, in which deferrals don't synthesize types */
  | IsInconsistentPartialAp(Typ.t, list(option(Typ.t))) /* Inconsistent partial application argument, in which deferrals don't synthesize types */
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
    | FreeVar
    | FreeDeferral
    | IsMeaninglessPartialAp
    // | IsInconsistentPartialApArg(_) => None
    | IsInconsistentPartialAp(_) => None
    | Common(self) => typ_of(ctx, self);

/* The self of a var depends on the ctx; if the
   lookup fails, it is a free variable */
let of_exp_var = (ctx: Ctx.t, name: Var.t): exp =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => FreeVar
  | Some(var) => Common(Just(var.typ))
  };

/* The self of a tag depends on the ctx, but a
   lookup failure doesn't necessarily means its
   free; it may be given a type analytically */
let of_tag = (ctx: Ctx.t, name: Tag.t): t =>
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
