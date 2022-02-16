include KindCore;

let consistent_subkind = (tyvars: TyVarCtx.t, k: t, k': t): bool =>
  switch (k, k') {
  | (KHole, _)
  | (_, KHole) => true
  | (Singleton(_), Type) => true
  | (Singleton(ty), Singleton(ty')) => HTyp.equivalent(tyvars, ty, ty')
  | (Type, Singleton(_)) => false
  | (Type, Type) => true
  };
