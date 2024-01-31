open Sexplib.Std;

/* SELF.re

   This module defines the SELF data structure, which represents
   the synthetic type information derivable from a term independent
   of the type expectation (i.e. MODE) of its syntactic context. This
   synethetic information is not entirely independent, in that it still
   uses the typing context passed down from the syntactic context.

   A term which from which a type can be derived in isolation, that is,
   that has a valid synthetic typing judgement, will generally have a SELF
   of Just(some_type). (The one current exception are the constructors of labelled
   sum types, which are handled specially as their synthetic type
   may be 'overwritten' by the analytic expectation)

   The other cases all represent states for which no single type can be
   derived, such as syntactic errors, or branching constructs which may
   have inconsistent types.

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type join_type =
  | Id
  | List;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Just(Typ.t) /* Just a regular type */
  | NoJoin(join_type, list(Typ.source)) /* Inconsistent types for e.g match, listlits */
  | BadToken(Token.t) /* Invalid expression token, treated as hole */
  | BadTrivAp(Typ.t) /* Trivial (nullary) ap on function that doesn't take triv */
  | BadSumAp(Typ.t)
  | IsMulti /* Multihole, treated as hole */
  | IsConstructor({
      name: Constructor.t,
      syn_ty: option(Typ.t),
    }); /* Constructors have special ana logic */

/* Expressions can also be free variables */
[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | Free(Var.t)
  | Common(t);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  | Common(t);

let join_of = (j: join_type, ty: Typ.t): Typ.t =>
  switch (j) {
  | Id => ty
  | List => List(ty)
  };

/* What the type would be if the position had been
   synthetic, so no hole fixing. Returns none if
   there's no applicable synthetic rule. */
let typ_of: (Ctx.t, t) => option(Typ.t) =
  _ctx =>
    fun
    | Just(typ) => Some(typ)
    | IsConstructor({syn_ty, _}) => syn_ty
    | BadToken(_)
    | BadTrivAp(_)
    | BadSumAp(_)
    | IsMulti
    | NoJoin(_) => None;

let typ_of_exp: (Ctx.t, exp) => option(Typ.t) =
  ctx =>
    fun
    | Free(_) => None
    | Common(self) => typ_of(ctx, self);

let typ_of_pat: (Ctx.t, pat) => option(Typ.t) =
  ctx =>
    fun
    | Common(self) => typ_of(ctx, self);

/* The self of a var depends on the ctx; if the
   lookup fails, it is a free variable */
let of_exp_var = (ctx: Ctx.t, name: Var.t): exp =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => Free(name)
  | Some(var) => Common(Just(var.typ))
  };

/* The self of a ctr depends on the ctx, but a
   lookup failure doesn't necessarily means its
   free; it may be given a type analytically */
let of_ctr = (ctx: Ctx.t, name: Constructor.t): t =>
  IsConstructor({
    name,
    syn_ty:
      switch (Ctx.lookup_ctr(ctx, name)) {
      | None => None
      | Some({typ, _}) => Some(typ)
      },
  });

let add_source = List.map2((id, ty) => Typ.{id, ty});

let match = (ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): t =>
  switch (Typ.join_all(~empty=Unknown(Internal), ctx, tys)) {
  | None => NoJoin(Id, add_source(ids, tys))
  | Some(ty) => Just(ty)
  };

let listlit = (~empty, ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): t =>
  switch (Typ.join_all(~empty, ctx, tys)) {
  | None => NoJoin(List, add_source(ids, tys))
  | Some(ty) => Just(List(ty))
  };

let list_concat = (ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): t =>
  switch (Typ.join_all(~empty=Unknown(Internal), ctx, tys)) {
  | None => NoJoin(List, add_source(ids, tys))
  | Some(ty) => Just(ty)
  };
