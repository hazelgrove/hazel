include KindCore;

type t = KindCore.t(Index.abs);

let singleton = (ty: HTyp.t): t => Singleton(HTyp.unsafe(ty));

let consistent_subkind = (tyctx: TyCtx.t, k: t, k': t): bool =>
  switch (k, k') {
  | (KHole, _)
  | (_, KHole) => true
  | (Singleton(_), T) => true
  | (Singleton(ty), Singleton(ty')) =>
    HTyp.equivalent(tyctx, HTyp.of_unsafe(ty), HTyp.of_unsafe(ty'))
  | (T, Singleton(_)) => false
  | (T, T) => true
  };

let canonical_type = (kind: t): HTyp.t =>
  KindCore.canonical_type(kind) |> HTyp.of_unsafe;
