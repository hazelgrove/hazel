/*
   Matched type judgements (▷): extract expected shapes from types after WHNF.
   Prefer these over ad hoc `Typ.term_of` switches — parens, unknowns, SynSwitch.
 */

open Util;
open Either;
open Typ;

type matcher = (Ctx.t, Typ.t) => option(list(Typ.t));

let synswitch = () => Unknown(SynSwitch) |> temp;
let internal = () => Unknown(Internal) |> temp;

let rec arrow: matcher =
  (ctx, ty) =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) => arrow(ctx, ty)
    | Arrow(ty_in, ty_out) => Some([ty_in, ty_out])
    | Unknown(SynSwitch) => Some([synswitch(), synswitch()])
    | _ => None
    };

let rec list: matcher =
  (ctx, ty) =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) => list(ctx, ty)
    | List(ty) => Some([ty])
    | Unknown(SynSwitch) => Some([synswitch()])
    | _ => None
    };

let rec poly: matcher =
  (ctx, ty) =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) => poly(ctx, ty)
    | Poly(_, body) => Some([body])
    | Unknown(SynSwitch) => Some([synswitch()])
    | _ => None
    };

let rec label: matcher =
  (ctx, ty) =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) => label(ctx, ty)
    | TupLabel(l, v) => Some([l, v])
    | Unknown(SynSwitch) => Some([synswitch(), synswitch()])
    | _ => None
    };

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

let prod = (arity): matcher =>
  (ctx, ty) =>
    switch (args(ctx, ty, arity)) {
    | L(tys) => Some(tys)
    | R(_) => None
    };

let tolerant = (f: matcher, ctx, ty): list(Typ.t) =>
  switch (f(ctx, ty)) {
  | Some(components) => components
  | None =>
    f(ctx, synswitch())
    |> Option.value(~default=[])
    |> List.map(_ => internal())
  };

let tolerant1 = (f: matcher, ctx, ty): Typ.t =>
  switch (tolerant(f, ctx, ty)) {
  | [t] => t
  | _ => internal()
  };

let tolerant2 = (f: matcher, ctx, ty): (Typ.t, Typ.t) =>
  switch (tolerant(f, ctx, ty)) {
  | [a, b] => (a, b)
  | _ => (internal(), internal())
  };

let strict1 = (f: matcher, ctx, ty): option(Typ.t) =>
  switch (f(ctx, ty)) {
  | Some([t]) => Some(t)
  | _ => None
  };

let strict2 = (f: matcher, ctx, ty): option((Typ.t, Typ.t)) =>
  switch (f(ctx, ty)) {
  | Some([a, b]) => Some((a, b))
  | _ => None
  };

let rec poly_pair = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => poly_pair(ctx, ty)
  | Poly(t, ty) => Some((Some(t), ty))
  | Unknown(SynSwitch) => Some((None, synswitch()))
  | _ => None
  };

let poly_pair_tolerant = (ctx, ty) =>
  poly_pair(ctx, ty) |> Option.value(~default=(None, internal()));

let rec prod_rearrange_strict:
  type a.
    (Ctx.t, list(a), a => option((string, a)), Typ.t, (string, a) => a) =>
    (list(a), option(list(Typ.t))) =
  (ctx: Ctx.t, es, get_label_es, ty: Typ.t, constructor) => {
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) =>
      prod_rearrange_strict(ctx, es, get_label_es, ty, constructor)
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
        Some(List.init(List.length(es), _ => synswitch())),
      )
    | _ => (es, None)
    };
  };

let prod_rearrange = (ctx, es, get_label_es, ty, constructor) => {
  let (es, tys_opt) =
    prod_rearrange_strict(ctx, es, get_label_es, ty, constructor);
  (
    es,
    tys_opt
    |> Option.value(~default=List.init(List.length(es), _ => internal())),
  );
};

/* A former pairs a matcher with the constructor it matches, so a query can be
   embedded into a type that does not decompose (an unknown callee's codomain
   is still an arrow's codomain). */
type former = {
  match_: matcher,
  build: list(Typ.t) => Typ.t,
};

let arrow_former = {
  match_: arrow,
  build:
    fun
    | [ty_in, ty_out] => Arrow(ty_in, ty_out) |> temp
    | _ => internal(),
};

let label_former = {
  match_: label,
  build:
    fun
    | [l, v] => TupLabel(l, v) |> temp
    | _ => internal(),
};

let prod_former = arity => {
  match_: prod(arity),
  build: tys => Prod(tys) |> temp,
};

let poly_former = {
  match_: poly,
  build:
    fun
    | [body] => Poly(EmptyHole |> TPat.fresh, body) |> temp
    | _ => internal(),
};

// Decomposes an instantiated type into the arguments for `binders`, bundled as
// the surface writes them, and rebuilds by substituting them back into `body`.
let instantiation_former = (~binders: list(TPat.t), ~body: Typ.t): former => {
  let bundle =
    fun
    | [arg] => arg
    | args => TypTuple(args) |> temp;
  let unbundle = (arg: Typ.t) =>
    switch (term_of(arg)) {
    | TypTuple(args) => args
    | _ => [arg]
    };
  {
    match_: (ctx, ty) =>
      Some([bundle(Typ.matched_instantiation(ctx, ~binders, ~body, ty))]),
    build:
      fun
      | [arg] when List.length(unbundle(arg)) == List.length(binders) =>
        Typ.subst_many(unbundle(arg), binders, body)
      | _ => internal(),
  };
};
