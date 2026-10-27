// Shape matching and type-level slicing: the one place query structure is read.

exception Incompatible_query(Typ.t);

let gap = Typ.gap;
let is_gap = Typ.is_gap;
let is_empty = Typ.is_empty;

let rec strip = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Parens(inner)
  | Projector(_, inner) => strip(inner)
  | _ => ty
  };

let whnf = (ctx: Ctx.t, ty: Typ.t): Typ.t =>
  strip(Typ.weak_head_normalize(ctx, ty));

let gaps = (arity: int): list(Typ.t) => List.init(arity, _ => gap);

let arrow = (ctx: Ctx.t, query: Typ.t): (Typ.t, Typ.t) =>
  switch (Typ.term_of(whnf(ctx, query))) {
  | Arrow(dom, cod) => (dom, cod)
  | _ => (gap, gap)
  };

let mk_arrow = (dom: Typ.t, cod: Typ.t): Typ.t => Typ.temp(Arrow(dom, cod));

let prod = (ctx: Ctx.t, query: Typ.t, arity: int): list(Typ.t) =>
  switch (Typ.term_of(whnf(ctx, query))) {
  | Prod(items) when List.length(items) == arity => items
  | _ when arity == 1 && !is_gap(query) => [query]
  | _ => gaps(arity)
  };

let mk_prod = (items: list(Typ.t)): Typ.t =>
  switch (items) {
  | [single] => single
  | _ => Typ.temp(Prod(items))
  };

let list_elem = (ctx: Ctx.t, query: Typ.t): Typ.t =>
  switch (Typ.term_of(whnf(ctx, query))) {
  | List(inner) => inner
  | _ => gap
  };

let mk_list = (inner: Typ.t): Typ.t => Typ.temp(List(inner));

let poly = (ctx: Ctx.t, query: Typ.t): (option(TPat.t), Typ.t) =>
  switch (Typ.term_of(whnf(ctx, query))) {
  | Poly(binder, body) => (Some(binder), body)
  | _ => (None, gap)
  };

let mk_poly = (binder: TPat.t, body: Typ.t): Typ.t =>
  Typ.temp(Poly(binder, body));

let tup_label = (ctx: Ctx.t, query: Typ.t): (option(string), Typ.t) =>
  switch (Typ.term_of(whnf(ctx, query))) {
  | TupLabel({term: Label(name), _}, inner) => (Some(name), inner)
  | _ => (None, query)
  };

let mk_tup_label = (name: string, inner: Typ.t): Typ.t =>
  Typ.temp(TupLabel(Typ.temp(Label(name)), inner));

let labelled = (ctx: Ctx.t, query: Typ.t, name: string): Typ.t => {
  let of_item = item =>
    switch (tup_label(ctx, item)) {
    | (Some(found), inner) when found == name => Some(inner)
    | _ => None
    };
  switch (Typ.term_of(whnf(ctx, query))) {
  | Prod(items) => List.find_map(of_item, items) |> Option.value(~default=gap)
  | _ => of_item(query) |> Option.value(~default=gap)
  };
};

let sum = (ctx: Ctx.t, query: Typ.t): option(Typ.sum_map) =>
  switch (Typ.term_of(whnf(ctx, query))) {
  | Sum(variants) => Some(variants)
  | _ => None
  };

let variant = (ctx: Ctx.t, query: Typ.t, name: string): option(Typ.t) =>
  switch (sum(ctx, query)) {
  | None => None
  | Some(variants) =>
    List.find_map(
      fun
      | ConstructorMap.Variant(found, _, payload) when found == name =>
        Some(Option.value(~default=gap, payload))
      | _ => None,
      variants,
    )
  };

let supplies = (ctx: Ctx.t, query: Typ.t, supplied: Typ.t): bool =>
  is_empty(Typ.subtract(ctx, query, supplied));

let residual = (ctx: Ctx.t, query: Typ.t, supplied: Typ.t): Typ.t => {
  let left = Typ.subtract(ctx, query, supplied);
  is_empty(left) ? gap : left;
};

let note = (id: Id.t): Id.Set.t =>
  id == Id.invalid ? Id.Set.empty : Id.Set.singleton(id);

let rec omit_typ = (ctx: Ctx.t, query: Typ.t, ty: Typ.t): Id.Set.t => {
  let query = strip(query);
  let query =
    switch (Typ.term_of(query), Typ.term_of(ty)) {
    | (TypFun(_, _), TypFun(_, _)) => query
    | (TypFun(_, body), _) => body
    | _ => query
    };
  switch (Typ.term_of(ty)) {
  | Parens(inner)
  | Projector(_, inner) when !is_gap(query) => omit_typ(ctx, query, inner)
  | Parens(inner)
  | Projector(_, inner) =>
    Id.Set.union(note(Typ.rep_id(ty)), note(Typ.rep_id(inner)))
  | Sig(_) => omit_sig(ctx, query, Typ.desugar_sig(ctx, ty))
  | TupLabel(_, value) =>
    let asked =
      switch (Typ.term_of(query)) {
      | TupLabel(_, inner) => inner
      | _ => query
      };
    is_gap(asked)
      ? note(Typ.rep_id(value)) : omit_typ(ctx, asked, value);
  | _ when is_gap(query) => note(Typ.rep_id(ty))
  | _ =>
    switch (Typ.children(ty), Typ.children(query)) {
    | (kids, parts)
        when kids != [] && List.length(kids) == List.length(parts) =>
      List.fold_left2(
        (acc, kid, part) => Id.Set.union(acc, omit_typ(ctx, part, kid)),
        Id.Set.empty,
        kids,
        parts,
      )
    | _ => Id.Set.empty
    }
  };
}

and omit_sig = (ctx: Ctx.t, query: Typ.t, desugared: Typ.t): Id.Set.t => {
  let item = (item: Typ.t): Id.Set.t =>
    switch (Typ.term_of(item)) {
    | TupLabel({term: Label(name), _}, value) =>
      let asked = labelled(ctx, query, name);
      is_gap(asked)
        ? note(Typ.rep_id(item)) : omit_typ(ctx, asked, value);
    | _ => Id.Set.empty
    };
  switch (Typ.term_of(desugared)) {
  | Prod(items) =>
    List.fold_left(
      (acc, part) => Id.Set.union(acc, item(part)),
      Id.Set.empty,
      items,
    )
  | _ => item(desugared)
  };
};

let omit_tpat = (query: Typ.t, binder: TPat.t): Id.Set.t =>
  is_empty(query) ? note(TPat.rep_id(binder)) : Id.Set.empty;
