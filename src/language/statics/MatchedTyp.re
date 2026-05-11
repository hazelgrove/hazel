/*
   Matched type judgements (▷): extract expected shapes from types after WHNF.
   Prefer these over ad hoc `Typ.term_of` switches — parens, unknowns, SynSwitch.
 */

open Util;
open Either;
open Typ;

let syn_switch = () => Unknown(SynSwitch |> Prov.fresh) |> temp;
let internal = () => Unknown(Internal |> Prov.fresh) |> temp;

let rec arrow = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => arrow(ctx, ty)
  | Arrow(ty_in, ty_out) => Some((ty_in, ty_out))
  | Unknown({term: SynSwitch, _}) => Some((syn_switch(), syn_switch()))
  | _ => None
  };

let arrow_tolerant = (ctx, ty) =>
  arrow(ctx, ty) |> Option.value(~default=(internal(), internal()));

let rec poly_pair = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => poly_pair(ctx, ty)
  | Poly(t, ty) => Some((Some(t), ty))
  | Unknown({term: SynSwitch, _}) => Some((None, syn_switch()))
  | _ => None
  };

let poly_pair_tolerant = (ctx, ty) =>
  poly_pair(ctx, ty) |> Option.value(~default=(None, internal()));

let rec prod_strict:
  type a.
    (Ctx.t, list(a), a => option((string, a)), Typ.t, (string, a) => a) =>
    (list(a), option(list(Typ.t))) =
  (ctx: Ctx.t, es, get_label_es, ty: Typ.t, constructor) => {
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) => prod_strict(ctx, es, get_label_es, ty, constructor)
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
    | Unknown({term: SynSwitch, _}) => (
        es,
        Some(List.init(List.length(es), _ => syn_switch())),
      )
    | _ => (es, None)
    };
  };

let prod = (ctx, es, get_label_es, ty, constructor) => {
  let (es, tys_opt) = prod_strict(ctx, es, get_label_es, ty, constructor);
  (
    es,
    tys_opt
    |> Option.value(
         ~default=List.init(List.length(es), _ => internal()),
       ),
  );
};

let rec list_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => list_strict(ctx, ty)
  | List(ty) => Some(ty)
  | Unknown({term: SynSwitch, _}) => Some(syn_switch())
  | _ => None
  };

let list_tolerant = (ctx, ty) =>
  list_strict(ctx, ty) |> Option.value(~default=internal());

let rec args = (ctx, ty, arity): Either.t('a, int) => {
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => args(ctx, ty, arity)
  | Prod(tys) when List.length(tys) == arity => L(tys)
  | Prod(tys) => R(List.length(tys))
  | _ when arity == 1 => L([ty])
  | Unknown(_) => L(List.init(arity, _ => internal()))
  | _ => R(1)
  };
};

let label = (ctx, ty): option((Typ.t, Typ.t)) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | TupLabel({term: Label(ml), _}, ty) => Some((Label(ml) |> temp, ty))
  | Unknown({term: SynSwitch, _}) => Some((syn_switch(), syn_switch()))
  | _ => None
  };
