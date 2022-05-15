include KindCore;

type t = KindCore.t(Index.absolute);

let singleton = (ty: HTyp.t): t => Singleton(HTyp.unsafe(ty));

let consistent_subkind = (ctx: Contexts.t, k: t, k': t): bool =>
  switch (k, k') {
  | (KHole, _)
  | (_, KHole) => true
  | (Singleton(_), T) => true
  | (Singleton(ty), Singleton(ty')) =>
    HTyp.equivalent(ctx, HTyp.of_unsafe(ty), HTyp.of_unsafe(ty'))
  | (T, Singleton(_)) => false
  | (T, T) => true
  };

let canonical_type = (kind: t): HTyp.t =>
  KindCore.canonical_type(kind) |> HTyp.of_unsafe;

let subst_tyvars = (k: t, tyvars: list((Index.Abs.t, HTyp.t))): t =>
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty) =>
    Singleton(HTyp.unsafe(HTyp.subst_tyvars(HTyp.of_unsafe(ty), tyvars)))
  };
