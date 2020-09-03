let consistent = (ctx: Contexts.t, k1: Kind.t, k2: Kind.t) =>
  switch (k1, k2) {
  | (KHole, _)
  | (_, KHole)
  | (Type, _)
  | (_, Type) => true
  | (Singleton(t1), Singleton(t2)) => HTypUtil.consistent(ctx, t1, t2)
  };
