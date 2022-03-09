[@deriving sexp]
type t = (TyVarCtx.t, HTyp.t);

let wrap = (ty: HTyp.t): t => (TyVarCtx.empty, ty);

let many = (tyvars: TyVarCtx.t, tys: list(HTyp.t)): list(t) =>
  List.map(ty => (tyvars, ty), tys);

let normalize = ((tyvars, ty): t): HTyp.normalized =>
  HTyp.normalize(tyvars, ty);

let head_normalize = ((tyvars, ty): t): HTyp.head_normalized =>
  HTyp.head_normalize(tyvars, ty);

let equivalent = (dty1: t, dty2: t): bool =>
  HTyp.normalized_equivalent(normalize(dty1), normalize(dty2));

let consistent = (dty1: t, dty2: t): bool =>
  HTyp.normalized_consistent(normalize(dty1), normalize(dty2));

let ground_cases_of = (dty: t): HTyp.ground_cases =>
  HTyp.ground_cases_of(normalize(dty));

let is_hole = (dty: t): bool =>
  HTyp.is_hole(HTyp.of_head_normalized(head_normalize(dty)));

let list = ((tyvars, ty): t): t => (tyvars, HTyp.list(ty));
