// Minimal assumptions: the gamma output of the slice judgement.

type key =
  | Val(string)
  | Ctr(string)
  | TVar(string)
  | Livelit(string);

let key_of = (entry: Ctx.entry): key =>
  switch (entry) {
  | VarEntry({name, _}) => Val(name)
  | ConstructorEntry({name, _}) => Ctr(name)
  | TVarEntry({name, _}) => TVar(name)
  | LivelitEntry({name, _}) => Livelit(name)
  };

let empty: Ctx.t = Ctx.empty;

let var = (~id=Id.invalid, name: Var.t, typ: Typ.t): Ctx.t =>
  Typ.is_gap(typ)
    ? empty
    : Ctx.extend(
        empty,
        VarEntry({
          name,
          id,
          typ,
          custom_statics: None,
        }),
      );

let ctr = (~id=Id.invalid, name: Var.t, typ: Typ.t): Ctx.t =>
  Typ.is_gap(typ)
    ? empty
    : Ctx.extend(
        empty,
        ConstructorEntry({
          name,
          id,
          typ,
          custom_statics: None,
        }),
      );

let alias = (~id=Id.invalid, name: string, typ: Typ.t): Ctx.t =>
  Ctx.extend(
    empty,
    TVarEntry({
      name,
      id,
      kind: Singleton(typ),
      typ_kind: TypKind.Type,
    }),
  );

let rec merge_binder = (left: TPat.t, right: TPat.t): TPat.t => {
  let named = (b: TPat.t) => TPat.tyvar_of_utpat(b) != None;
  switch (left.term, right.term) {
  | (Tuple(lefts), Tuple(rights))
      when List.length(lefts) == List.length(rights) => {
      ...left,
      term: Tuple(List.map2(merge_binder, lefts, rights)),
    }
  | (Parens(inner), _) => {...left, term: Parens(merge_binder(inner, right))}
  | (_, Parens(inner)) => merge_binder(left, inner)
  | (_, _) => named(left) ? left : right
  };
};

let rec merge_definition = (ctx: Ctx.t, left: Typ.t, right: Typ.t): Typ.t =>
  switch (Typ.term_of(left), Typ.term_of(right)) {
  | (TypFun(left_binder, left_body), TypFun(right_binder, right_body)) =>
    Typ.temp(
      TypFun(
        merge_binder(left_binder, right_binder),
        merge_definition(ctx, left_body, right_body),
      ),
    )
  | (_, _) => Typ.meet_gap(ctx, left, right)
  };

let combine = (ctx: Ctx.t, left: Ctx.entry, right: Ctx.entry): Ctx.entry =>
  switch (left, right) {
  | (VarEntry(l), VarEntry(r)) =>
    VarEntry({...l, typ: Typ.meet_gap(ctx, l.typ, r.typ)})
  | (ConstructorEntry(l), ConstructorEntry(r)) =>
    ConstructorEntry({...l, typ: Typ.meet_gap(ctx, l.typ, r.typ)})
  | (
      TVarEntry({kind: Singleton(l_ty), _} as l),
      TVarEntry({kind: Singleton(r_ty), _}),
    ) =>
    TVarEntry({...l, kind: Singleton(merge_definition(ctx, l_ty, r_ty))})
  | (_, _) => left
  };

let join = (ctx: Ctx.t, left: Ctx.t, right: Ctx.t): Ctx.t =>
  List.fold_left(
    (acc: Ctx.t, entry) => {
      let k = key_of(entry);
      switch (List.partition(e => key_of(e) == k, acc.entries)) {
      | ([], rest) => {...acc, entries: [entry, ...rest]}
      | ([held, ...spare], rest) => {
          ...acc,
          entries: [combine(ctx, held, entry), ...spare @ rest],
        }
      };
    },
    left,
    right.entries,
  );

let join_all = (ctx: Ctx.t, gammas: list(Ctx.t)): Ctx.t =>
  List.fold_left(join(ctx), empty, gammas);

let demand = (gamma: Ctx.t, name: Var.t): Typ.t =>
  switch (Ctx.lookup_var(gamma, name)) {
  | Some({typ, _}) => typ
  | None => Typ.gap
  };

let ctr_demand = (gamma: Ctx.t, name: Var.t): Typ.t =>
  switch (Ctx.lookup_ctr(gamma, name)) {
  | Some({typ, _}) => typ
  | None => Typ.gap
  };

let alias_demand = (gamma: Ctx.t, name: string): Typ.t =>
  switch (Ctx.lookup_alias(gamma, name)) {
  | Some(typ) => typ
  | None => Typ.gap
  };

let without = (gamma: Ctx.t, names: list(Var.t)): Ctx.t => {
  ...gamma,
  entries:
    List.filter(
      entry =>
        switch (entry) {
        | Ctx.VarEntry({name, _}) => !List.mem(name, names)
        | _ => true
        },
      gamma.entries,
    ),
};

let without_tvars = (gamma: Ctx.t, names: list(string)): Ctx.t => {
  ...gamma,
  entries:
    List.filter(
      entry =>
        switch (entry) {
        | Ctx.TVarEntry({name, _}) => !List.mem(name, names)
        | _ => true
        },
      gamma.entries,
    ),
};
