/*
   Matched Type Utilities: This file collect judgements usually written as ▷ in papers.

   It is almost always better to use one of these than to directly check if a type is of
   the correct form, because it's easy to forget cases (e.g. parens, unknowns) when
   doing so.

   The default versions return an option type, which is None if the type does not match.
   The tolerant versions return a default "unknown internal" type when the type does not match.
 */

open Util;
open Typ;
open Grammar;
open Either;

// Arrow: normal case
let rec arrow = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => arrow(ctx, ty)
  | Arrow(ty_in, ty_out) => Some((ty_in, ty_out))
  | Unknown(SynSwitch) =>
    Some((Unknown(SynSwitch) |> temp, Unknown(SynSwitch) |> temp))
  | _ => None
  };

let arrow_tolerant = (ctx, ty) =>
  arrow(ctx, ty)
  |> Option.value(
       ~default=(Unknown(Internal) |> temp, Unknown(Internal) |> temp),
     );

/* Poly: requires alpha-equivalence logic
   The "tvar" argument specifies the type variable you want to be used in
   the result. If the poly type uses a different type variable name, it will be
   alpha-renamed to use the provided one. If None is provided, no renaming is done. */
let rec poly = (ctx, replacement, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => poly(ctx, replacement, ty)
  | Unknown(SynSwitch) => Some(Unknown(SynSwitch) |> temp)
  | Poly(poly_tvar, ty) =>
    switch (replacement) {
    | Some(replacement) => Some(Typ.subst(replacement, poly_tvar, ty))
    | None =>
      switch (TPat.tyvar_of_utpat(poly_tvar)) {
      | Some(name) =>
        Some(
          Typ.subst(
            Var(Ctx.free_tvar_name(ctx, name)) |> Typ.temp,
            poly_tvar,
            ty,
          ),
        )
      | None => Some(ty)
      }
    }
  | _ => None
  };

let poly_tolerant = (ctx, replacement, ty) =>
  poly(ctx, replacement, ty)
  |> Option.value(~default=Unknown(Internal) |> temp);

let rec prod:
  type a.
    (Ctx.t, list(a), a => option((string, a)), Typ.t, (string, a) => a) =>
    (list(a), option(list(Typ.t))) =
  (ctx: Ctx.t, es, get_label_es, ty: Typ.t, constructor) => {
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) => prod(ctx, es, get_label_es, ty, constructor)
    | Prod(tys: list(Typ.t)) =>
      if (List.length(es) != List.length(tys)) {
        (es, None);
      } else {
        (
          LabeledTuple.rearrange(
            match_tup_label,
            get_label_es,
            tys,
            es,
            constructor,
          ),
          Some(tys),
        );
      }
    | Unknown(SynSwitch) => (
        es,
        Some(List.init(List.length(es), _ => Unknown(SynSwitch) |> temp)),
      )
    | _ => (es, None)
    };
  };

let prod_tolerant = (ctx, es, get_label_es, ty, constructor) => {
  let (es, tys_opt) = prod(ctx, es, get_label_es, ty, constructor);
  (
    es,
    tys_opt
    |> Option.value(
         ~default=List.init(List.length(es), _ => Unknown(Internal) |> temp),
       ),
  );
};

let rec list = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => list(ctx, ty)
  | List(ty) => Some(ty)
  | Unknown(SynSwitch) => Some(Unknown(SynSwitch) |> temp)
  | _ => None
  };

let list_tolerant = (ctx, ty) =>
  list(ctx, ty) |> Option.value(~default=Unknown(Internal) |> temp);

let rec args = (ctx, ty, arity): Either.t('a, int) => {
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => args(ctx, ty, arity)
  | Prod(tys) when List.length(tys) == arity => L(tys)
  | Prod(tys) => R(List.length(tys))
  | _ when arity == 1 => L([ty])
  | Unknown(_) => L(List.init(arity, _ => Unknown(Internal) |> temp))
  | _ => R(1)
  };
};

let label = (ctx, ty): option((Typ.t, Typ.t)) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | TupLabel({term: Label(ml), _}, ty) => Some((Label(ml) |> temp, ty))
  | Unknown(SynSwitch) =>
    Some((Unknown(SynSwitch) |> temp, Unknown(SynSwitch) |> temp))
  | _ => None
  };
