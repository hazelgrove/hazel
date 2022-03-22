include KindCore;

let singleton = (ty: HTyp.t): t => Singleton(HTyp.unsafe(ty));

let consistent_subkind = (tyvars: TyVarCtx.t, k: t, k': t): bool =>
  switch (k, k') {
  | (KHole, _)
  | (_, KHole) => true
  | (Singleton(_), Type) => true
  | (Singleton(ty), Singleton(ty')) =>
    HTyp.equivalent(tyvars, HTyp.of_unsafe(ty), HTyp.of_unsafe(ty'))
  | (Type, Singleton(_)) => false
  | (Type, Type) => true
  };

let canonical_type = (kind: t): HTyp.t =>
  KindCore.canonical_type(kind) |> HTyp.of_unsafe;
