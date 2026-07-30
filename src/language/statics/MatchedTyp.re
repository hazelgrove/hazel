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

let prod_extension_former = (ctx: Ctx.t, left: Typ.t, right: Typ.t): former => {
  let entry = ty =>
    switch (match_tup_label(ty)) {
    | Some((label, payload)) => (Some(label), payload)
    | None => (None, ty)
    };
  let entries = ty =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Prod(ts) => List.map(entry, ts)
    | _ => []
    };
  let wrap =
    fun
    | (Some(label), payload) =>
      TupLabel(Label(label) |> temp, payload) |> temp
    | (None, payload) => payload;
  let product = entries => Prod(List.map(wrap, entries)) |> temp;
  let left_entries = entries(left);
  let right_entries = entries(right);
  let output_entries = LabeledTuple.extension(left_entries, right_entries);
  let origins =
    LabeledTuple.extension(
      List.mapi((i, (label, _)) => (label, (true, i)), left_entries),
      List.mapi((i, (label, _)) => (label, (false, i)), right_entries),
    );
  let split = (side, original, routed) =>
    List.mapi(
      (index, (label, _)) => {
        let payload =
          List.find_map(
            (((_, (from_left, source)), (_, query))) =>
              from_left == side && source == index ? Some(query) : None,
            routed,
          )
          |> Option.value(~default=Typ.gap);
        (label, payload);
      },
      original,
    );
  {
    match_: (ctx, query) =>
      switch (term_of(weak_head_normalize(ctx, query))) {
      | Unknown(SynSwitch) => Some([synswitch(), synswitch()])
      | Prod(ts) when List.length(ts) == List.length(output_entries) =>
        let ts =
          LabeledTuple.rearrange(
            match_tup_label,
            match_tup_label,
            List.map(wrap, output_entries),
            ts,
            (label, payload) =>
            wrap((Some(label), payload))
          );
        let routed = List.combine(origins, List.map(entry, ts));
        Some([
          split(true, left_entries, routed) |> product,
          split(false, right_entries, routed) |> product,
        ]);
      | _ => None
      },
    build:
      fun
      | [left, right] =>
        product(LabeledTuple.extension(entries(left), entries(right)))
      | _ => internal(),
  };
};

let bundle_args =
  fun
  | [ty] => ty
  | tys => Prod(tys) |> temp;

let deferred_ap_former = (ctx: Ctx.t, deferred: list(bool)): former => {
  let rec refill = (deferred, supplied) =>
    switch (deferred, supplied) {
    | ([], _) => []
    | ([true, ...rest], [ty, ...supplied]) => [
        ty,
        ...refill(rest, supplied),
      ]
    | ([_, ...rest], supplied) => [gap, ...refill(rest, supplied)]
    };
  let remaining = List.length(List.filter(Fun.id, deferred));
  let kept = inputs =>
    List.combine(deferred, inputs)
    |> List.filter_map(((keep, input)) => keep ? Some(input) : None);
  let partial = (inputs, codomain) =>
    Arrow(bundle_args(kept(inputs)), codomain) |> temp;
  {
    match_: (ctx, ty) =>
      switch (strict2(arrow, ctx, ty)) {
      | Some((domain, codomain)) =>
        switch (args(ctx, domain, List.length(deferred))) {
        | L(inputs) => Some([partial(inputs, codomain)])
        | R(_) => None
        }
      | None => None
      },
    build:
      fun
      | [partial] => {
          let (domain, codomain) = tolerant2(arrow, ctx, partial);
          let supplied = tolerant(prod(remaining), ctx, domain);
          Arrow(bundle_args(refill(deferred, supplied)), codomain) |> temp;
        }
      | _ => internal(),
  };
};

// Matches an instantiated body back to its surface type arguments.
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
    match_: (ctx, ty) => {
      let names = List.filter_map(TPat.tyvar_of_utpat, binders);
      let (_, constraints) = Typ.collect_constraints(ctx, names, body, ty);
      let args =
        List.map(
          binder =>
            switch (TPat.tyvar_of_utpat(binder)) {
            | None => Typ.gap
            | Some(name) =>
              constraints
              |> List.filter_map(((found, query)) =>
                   found == name ? Some(query) : None
                 )
              |> Typ.meet_gap_all(ctx)
            },
          binders,
        );
      Some([bundle(args)]);
    },
    build:
      fun
      | [arg] when List.length(unbundle(arg)) == List.length(binders) =>
        Typ.subst_many(unbundle(arg), binders, body)
      | _ => internal(),
  };
};
