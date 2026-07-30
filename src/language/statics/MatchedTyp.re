/*
   Matched type judgements (▷): extract expected shapes from types after WHNF.
   Prefer these over ad hoc `Typ.term_of` switches — parens, unknowns, SynSwitch.
 */

open Util;
open Either;
open Typ;

type matcher = (Ctx.t, Typ.t) => option(list(Typ.t));

type former = {
  arity: int,
  fixed: list(bool),
  parts: Typ.t => option(list(Typ.t)),
  whole: list(Typ.t) => Typ.t,
};

type formation = {
  former,
  components: list(Typ.t),
};

let form = (former, components) => {
  former,
  components,
};
let formed_type = ({former, components}: formation) =>
  former.whole(components);
let reform = (former, ty) =>
  switch (former.parts(ty)) {
  | Some(components) => form(former, components)
  | None => failwith("type does not match former")
  };

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

let make_former_with_fixed = (~arity, ~fixed, ~parts, ~whole): former => {
  arity,
  fixed,
  parts,
  whole,
};

let make_former = (~arity, ~parts, ~whole): former =>
  make_former_with_fixed(~arity, ~fixed=[], ~parts, ~whole);

let identity_former =
  make_former(
    ~arity=1,
    ~parts=ty => Some([ty]),
    ~whole=
      fun
      | [ty] => ty
      | _ => internal(),
  );

let identity = ty => form(identity_former, [ty]);

let arrow_former =
  make_former(
    ~arity=2,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Arrow(left, right) => Some([left, right])
        | _ => None
        },
    ~whole=
      fun
      | [left, right] => Arrow(left, right) |> temp
      | _ => internal(),
  );

let list_former =
  make_former(
    ~arity=1,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | List(inner) => Some([inner])
        | _ => None
        },
    ~whole=
      fun
      | [inner] => List(inner) |> temp
      | _ => internal(),
  );

let label_former =
  make_former_with_fixed(
    ~arity=2,
    ~fixed=[true, false],
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | TupLabel(label, value) => Some([label, value])
        | _ => None
        },
    ~whole=
      fun
      | [label, value] => TupLabel(label, value) |> temp
      | _ => internal(),
  );

let prod_former = arity =>
  make_former(
    ~arity,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Prod(items) when List.length(items) == arity => Some(items)
        | _ => None
        },
    ~whole=items => Prod(items) |> temp,
  );

let tuple_former = (~duplicate_labels, items) => {
  let (_, slots) =
    List.fold_left_map(
      (seen, item) =>
        switch (match_tup_label(item)) {
        | Some((label, _)) when List.mem(label, duplicate_labels) =>
          List.mem(label, seen)
            ? (seen, (false, None))
            : ([label, ...seen], (true, Some(label)))
        | _ => (seen, (true, None))
        },
      [],
      items,
    );
  let output_arity = List.filter(((keep, _)) => keep, slots) |> List.length;
  make_former(
    ~arity=List.length(items),
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Prod(outputs) when List.length(outputs) == output_arity =>
          let (_, components) =
            List.fold_left_map(
              (outputs, (keep, _)) =>
                if (keep) {
                  switch (outputs) {
                  | [output, ...outputs] => (outputs, output)
                  | [] => ([], gap)
                  };
                } else {
                  (outputs, gap);
                },
              outputs,
              slots,
            );
          Some(components);
        | _ => None
        },
    ~whole=
      components =>
        if (List.length(components) != List.length(slots)) {
          internal();
        } else {
          List.map2(
            ((keep, duplicate), component) =>
              if (!keep) {
                None;
              } else {
                switch (duplicate) {
                | None => Some(component)
                | Some(label) =>
                  Some(
                    is_empty(component)
                      ? component
                      : TupLabel(
                          Label(label) |> temp,
                          Unknown(Internal) |> temp,
                        )
                        |> temp,
                  )
                };
              },
            slots,
            components,
          )
          |> List.filter_map(Fun.id)
          |> (items => Prod(items) |> temp);
        },
  );
};

let poly_former = binder =>
  make_former(
    ~arity=1,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Poly(_, body) => Some([body])
        | _ => None
        },
    ~whole=
      fun
      | [body] => Poly(binder, body) |> temp
      | _ => internal(),
  );

let parens_former =
  make_former(
    ~arity=1,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Parens(inner) => Some([inner])
        | _ => None
        },
    ~whole=
      fun
      | [inner] => Parens(inner) |> temp
      | _ => internal(),
  );

let projector_former = data =>
  make_former(
    ~arity=1,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Projector(_, inner) => Some([inner])
        | _ => None
        },
    ~whole=
      fun
      | [inner] => Projector(data, inner) |> temp
      | _ => internal(),
  );

let typ_param_ap_former =
  make_former(
    ~arity=2,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | TypParamAp(fn, arg) => Some([fn, arg])
        | _ => None
        },
    ~whole=
      fun
      | [fn, arg] => TypParamAp(fn, arg) |> temp
      | _ => internal(),
  );

let typ_tuple_former = arity =>
  make_former(
    ~arity,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | TypTuple(items) when List.length(items) == arity => Some(items)
        | _ => None
        },
    ~whole=items => TypTuple(items) |> temp,
  );

// Query inversion for explicit instantiation is handled by Slice.
let typ_ap_former = (~binders, ~body) =>
  make_former(
    ~arity=List.length(binders),
    ~parts=_ => None,
    ~whole=
      args =>
        List.length(args) == List.length(binders)
          ? Typ.subst_many(args, binders, body) : internal(),
  );

let prod_projection_former =
  make_former_with_fixed(
    ~arity=2,
    ~fixed=[false, true],
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | ProdProjection(product, label) => Some([product, label])
        | _ => None
        },
    ~whole=
      fun
      | [product, label] => ProdProjection(product, label) |> temp
      | _ => internal(),
  );

let prod_extension_node_former =
  make_former(
    ~arity=2,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | ProdExtension(left, right) => Some([left, right])
        | _ => None
        },
    ~whole=
      fun
      | [left, right] => ProdExtension(left, right) |> temp
      | _ => internal(),
  );

let typ_fun_former = binder =>
  make_former(
    ~arity=1,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | TypFun(_, body) => Some([body])
        | _ => None
        },
    ~whole=
      fun
      | [body] => TypFun(binder, body) |> temp
      | _ => internal(),
  );

let rec_former = binder =>
  make_former(
    ~arity=1,
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Rec(_, body) => Some([body])
        | _ => None
        },
    ~whole=
      fun
      | [body] => Rec(binder, body) |> temp
      | _ => internal(),
  );

let sum_former = (shape_variants: list(ConstructorMap.variant(Typ.t))) => {
  let components = variants =>
    List.concat_map(
      fun
      | ConstructorMap.Variant(name, ann, payload) => [
          IdTagged.mk_internal(ann.ids, Var(name): term),
          Option.value(~default=gap, payload),
        ]
      | ConstructorMap.BadEntry(inner) => [inner, gap],
      variants,
    );
  let whole = replacements => {
    let variant = (ann, named, payload) =>
      switch (term_of(named)) {
      | Var(name) => ConstructorMap.Variant(name, ann, payload)
      | _ =>
        ConstructorMap.BadEntry(
          switch (payload) {
          | Some(payload) => payload
          | None => named
          },
        )
      };
    let rec refill = (variants, replacements) =>
      switch (variants, replacements) {
      | ([], []) => Some([])
      | (
          [ConstructorMap.Variant(_, ann, None), ...rest],
          [named, _, ...replacements],
        ) =>
        refill(rest, replacements)
        |> Option.map(List.cons(variant(ann, named, None)))
      | (
          [ConstructorMap.Variant(_, ann, Some(_)), ...rest],
          [named, payload, ...replacements],
        ) =>
        refill(rest, replacements)
        |> Option.map(List.cons(variant(ann, named, Some(payload))))
      | ([ConstructorMap.BadEntry(_), ...rest], [inner, _, ...replacements]) =>
        refill(rest, replacements)
        |> Option.map(List.cons(ConstructorMap.BadEntry(inner)))
      | _ => None
      };
    refill(shape_variants, replacements)
    |> Option.map(variants => Sum(variants) |> temp)
    |> Option.value(~default=internal());
  };
  make_former_with_fixed(
    ~arity=2 * List.length(shape_variants),
    ~fixed=List.concat_map(_ => [true, false], shape_variants),
    ~parts=
      ty =>
        switch (term_of(ty)) {
        | Sum(variants)
            when List.length(variants) == List.length(shape_variants) =>
          Some(components(variants))
        | _ => None
        },
    ~whole,
  );
};

let sum_payload_former =
    (~shape: Typ.t, ~expanded: Typ.t, ctr: Constructor.t): former => {
  let payload_of = variants =>
    List.find_map(
      fun
      | ConstructorMap.Variant(name, _, payload)
          when Constructor.equal(name, ctr) =>
        Some(payload |> Option.value(~default=gap))
      | _ => None,
      variants,
    )
    |> Option.value(~default=gap);
  let original_payload =
    switch (term_of(expanded)) {
    | Sum(variants) => payload_of(variants)
    | _ => gap
    };
  make_former(
    ~arity=1,
    ~parts=
      ty => {
        let ty = Typ.fast_equal(ty, shape) ? expanded : ty;
        switch (term_of(ty)) {
        | Sum(variants) => Some([payload_of(variants)])
        | _ => None
        };
      },
    ~whole=
      fun
      | [payload] when Typ.fast_equal(payload, original_payload) => shape
      | [payload] =>
        switch (term_of(expanded)) {
        | Sum(variants) =>
          Sum(
            List.map(
              fun
              | ConstructorMap.Variant(name, ann, _)
                  when Constructor.equal(name, ctr) =>
                ConstructorMap.Variant(name, ann, Some(payload))
              | variant => variant,
              variants,
            ),
          )
          |> temp
        | _ => gap
        }
      | _ => gap,
  );
};

let prod_extension_former = (left: Typ.t, right: Typ.t): former => {
  let entry = ty =>
    switch (match_tup_label(ty)) {
    | Some((label, payload)) => (Some(label), payload)
    | None => (None, ty)
    };
  let entries = ty =>
    switch (term_of(ty)) {
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
  let parts = query =>
    switch (term_of(query)) {
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
    };
  let build =
    fun
    | [left, right] =>
      product(LabeledTuple.extension(entries(left), entries(right)))
    | _ => internal();
  make_former(~arity=2, ~parts, ~whole=build);
};

let bundle_args =
  fun
  | [ty] => ty
  | tys => Prod(tys) |> temp;

let deferred_ap_former = (deferred: list(bool)): former => {
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
  let split_inputs = (arity, ty) =>
    switch (arity, term_of(ty)) {
    | (1, _) => Some([ty])
    | (arity, Prod(inputs)) when List.length(inputs) == arity =>
      Some(inputs)
    | _ => None
    };
  let parts = ty =>
    switch (term_of(ty)) {
    | Arrow(domain, codomain) =>
      split_inputs(List.length(deferred), domain)
      |> Option.map(inputs => [partial(inputs, codomain)])
    | _ => None
    };
  let whole =
    fun
    | [partial] => {
        switch (term_of(partial)) {
        | Arrow(domain, codomain) =>
          switch (split_inputs(remaining, domain)) {
          | Some(supplied) =>
            Arrow(bundle_args(refill(deferred, supplied)), codomain) |> temp
          | None => internal()
          }
        | _ => internal()
        };
      }
    | _ => internal();
  make_former(~arity=1, ~parts, ~whole);
};
