[@deriving sexp]
type t = (Context.t, HTyp.t);

let wrap = (ty: HTyp.t): t => (Context.initial, ty);

let many = (ctx: Context.t, tys: list(HTyp.t)): list(t) =>
  List.map(ty => (ctx, ty), tys);

let normalize = ((ctx, ty): t): HTyp.normalized => HTyp.normalize(ctx, ty);

let head_normalize = ((ctx, ty): t): HTyp.head_normalized =>
  HTyp.head_normalize(ctx, ty);

let equivalent = (dty1: t, dty2: t): bool =>
  HTyp.normalized_equivalent(normalize(dty1), normalize(dty2));

let consistent = (dty1: t, dty2: t): bool =>
  HTyp.normalized_consistent(normalize(dty1), normalize(dty2));

let ground_cases_of = (dty: t): HTyp.ground_cases =>
  HTyp.ground_cases_of(normalize(dty));

let is_hole = (dty: t): bool =>
  HTyp.is_hole(HTyp.of_head_normalized(head_normalize(dty)));

let list = ((tyvars, ty): t): t => (tyvars, HTyp.list(ty));
