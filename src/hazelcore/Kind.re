include KindCore;

type t = KindCore.t(Index.absolute);

let singleton = (ty: HTyp.t): t => Singleton(HTyp.unsafe(ty));

let consistent_subkind = (ctx: Contexts.t, k: t, k': t): bool =>
  switch (k, k') {
  | (KHole, _)
  | (_, KHole) => true
  | (Singleton(_), T) => true
  | (Singleton(ty), Singleton(ty')) =>
    Contexts.equivalent(ctx, HTyp.of_unsafe(ty), HTyp.of_unsafe(ty'))
  | (T, Singleton(_)) => false
  | (T, T) => true
  };

let canonical_type = (kind: t): HTyp.t =>
  KindCore.canonical_type(kind) |> HTyp.of_unsafe;

let eliminate_tyvars = (k: t, tyvars: list((TyVar.t, t))): t => {
  let rec do_htyp = ty =>
    switch (ty) {
    | HTypSyntax.TyVar(i, _) =>
      switch (List.nth_opt(tyvars, Index.Abs.to_int(i))) {
      | Some((_, k)) => KindCore.canonical_type(k)
      | None => ty
      }
    | TyVarHole(_)
    | Hole
    | Int
    | Float
    | Bool => ty
    | Arrow(ty1, ty2) => Arrow(do_htyp(ty1), do_htyp(ty2))
    | Sum(tyL, tyR) => Sum(do_htyp(tyL), do_htyp(tyR))
    | Prod(tys) => Prod(List.map(do_htyp, tys))
    | List(ty) => List(do_htyp(ty))
    };
  switch (k) {
  | KHole
  | T => k
  | Singleton(ty) =>
    let ty = do_htyp(ty);
    Singleton(ty);
  };
};
