open Util;

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
  | Duplicate(LabeledTuple.label, t) /* Duplicate label, marked as duplicate */
  | BadToken(string) /* Invalid expression token, continues with undefined behavior */
  | BadLabel(Any.t) /* TupLabel label component is not a valid Label*/
  | InvalidLabel(LabeledTuple.label) /* Invalid label in a labeled tuple */
  | TupleLabelError({
      malformed_labels: list(Any.t), // Labels that are not of the right syntactic form
      duplicate_labels: list(LabeledTuple.label),
      invalid_labels: list(LabeledTuple.label), // Labels that are present but aren't present in the analyzed type
      typ: Typ.t,
    }) /* Tuple/TupLabel contains malformed labels, duplicate labels, and/or invalid labels */
  | IsMulti /* Multihole, treated as hole */
  | FreeConstructor(Constructor.t); /* Constructor not bound in context or ana type */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_partial_ap =
  | NoDeferredArgs
  | ArityMismatch({
      expected: int,
      actual: int,
    });

/* Expressions can also be free variables */
[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | Free(Var.t)
  | InexhaustiveMatch(exp)
  | IsDeferral(Exp.deferral_position)
  | IsBadPartialAp(error_partial_ap)
  | Common(t)
  | InvalidUseMode({
      bad_typ: Typ.t,
      inner_typ: Typ.t,
    })
  | IsLivelitName({
      name: string,
      exp_t: Typ.t,
    })
  | BadTrivAp(Typ.t) /* Trivial (nullary) ap on function that doesn't take triv */
  | WantTuple /* Want a Tuple, found not-tuple */
  | LabelNotFound(LabeledTuple.label, list(LabeledTuple.label)) /* Currently used by the dot operator for a label not found */
  | BadOperator(string) /* Invalid operator, continues with undefined behavior */
  | BadLivelitModel(Typ.t); /* Livelit model type is not valid */

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  | Redundant(pat)
  | ExpectedConstructor(pat)
  | Common(t);

let join_of = (j: join_type, ty: Typ.t): Typ.t =>
  switch (j) {
  | Id => ty
  | List => List(ty) |> Typ.fresh
  };

/* What the type would be if the position had been
   synthetic, so no hole fixing. Returns none if
   there's no applicable synthetic rule. */
let typ_of: t => option(Typ.t) =
  fun
  | Just(typ)
  | Duplicate(_, Just(typ))
  | TupleLabelError({typ, _}) => Some(typ)
  | FreeConstructor(name) =>
    Some(
      Sum([
        ConstructorMap.Variant(name, [Id.invalid], None),
        ConstructorMap.BadEntry(Unknown(Internal) |> Typ.temp),
      ])
      |> Typ.temp,
    )
  | BadToken(_)
  | IsMulti
  | Duplicate(_)
  | BadLabel(_)
  | InvalidLabel(_)
  | NoJoin(_) => None;

let typ_of_exp: exp => option(Typ.t) =
  fun
  | Free(_)
  | InexhaustiveMatch(_)
  | IsDeferral(_)
  | IsBadPartialAp(_)
  | BadTrivAp(_)
  | LabelNotFound(_)
  | BadOperator(_)
  | WantTuple => None
  | Common(self) => typ_of(self)
  | InvalidUseMode({inner_typ, _}) => Some(inner_typ)
  | IsLivelitName({exp_t, _}) => Some(exp_t)
  | BadLivelitModel(typ) => Some(typ);

let rec typ_of_pat: pat => option(Typ.t) =
  fun
  | Redundant(pat) => typ_of_pat(pat)
  | ExpectedConstructor(pat) => typ_of_pat(pat)
  | Common(self) => typ_of(self);

/* The self of a var and livelit depends on the ctx; if the
   lookup fails, it is a free variable */
let of_exp_var = (ctx: Ctx.t, name: Var.t): exp =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => Free(name)
  | Some(var) => Common(Just(var.typ))
  };

let ctr_ana_typ =
    (ctx: Ctx.t, ty_ana: Typ.t, ctr: Constructor.t): option(Typ.t) => {
  /* If a ctr is being analyzed against (an arrow type returning)
     a sum type having that ctr as a variant, we consider the
     ctr's type to be determined by the sum type */
  OptUtil.Syntax.(
    switch (ty_ana) {
    | {term: Arrow(_, ty_out), _} =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty_out);
      let* ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => None
      | Some(ty_in) => Some(Arrow(ty_in, ty_out) |> Typ.temp)
      };
    | _ =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty_ana);
      let+ ty_entry = ConstructorMap.get_entry(ctr, ctrs);
      switch (ty_entry) {
      | None => ty_ana
      | Some(ty_in) => Arrow(ty_in, ty_ana) |> Typ.temp
      };
    }
  );
};

let of_exp_livelit_name = (ctx: Ctx.t, name: string): exp => {
  let res = Ctx.lookup_livelit(ctx, name);
  switch (res) {
  | None => Free(name)
  | Some(livelit) =>
    IsLivelitName({
      name: livelit.name,
      exp_t: livelit.expansion_t,
    })
  };
};

let of_ctr =
    (ctx: Ctx.t, name: Constructor.t, ana: Typ.t, ty: option(option(Typ.t)))
    : t => {
  // (1) check to see if type already assigned (e.g. if we are doing statics for results)
  switch (ty) {
  | Some(Some(ty)) => Just(ty)
  | Some(None) => FreeConstructor(name)
  | None =>
    // (2) check to see if constructor appears in ana type
    switch (ctr_ana_typ(ctx, ana, name)) {
    | Some(ty) => Just(ty)
    | None =>
      // (3) check to see if constructor appears in ctx
      switch (Ctx.lookup_ctr(ctx, name)) {
      | Some({typ, _}) => Just(typ)
      | None => FreeConstructor(name)
      }
    }
  };
};

let add_source =
  List.map2((id, ty) =>
    Typ.{
      id,
      ty,
    }
  );

let match = (ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): t =>
  switch (Typ.join_all(~empty=Unknown(Internal) |> Typ.fresh, ctx, tys)) {
  | None => NoJoin(Id, add_source(ids, tys))
  | Some(ty) => Just(ty)
  };

let listlit = (~empty, ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): t =>
  switch (Typ.join_all(~empty, ctx, tys)) {
  | None => NoJoin(List, add_source(ids, tys))
  | Some(ty) => Just(List(ty) |> Typ.fresh)
  };

let list_concat = (ctx: Ctx.t, tys: list(Typ.t), ids: list(Id.t)): t =>
  switch (Typ.join_all(~empty=Unknown(Internal) |> Typ.fresh, ctx, tys)) {
  | None => NoJoin(List, add_source(ids, tys))
  | Some(ty) => Just(ty)
  };
