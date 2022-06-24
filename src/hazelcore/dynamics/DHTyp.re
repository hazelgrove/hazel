[@deriving sexp]
type t = (Context.t, HTyp.t);

let many = (ctx: Context.t, tys: list(HTyp.t)): list(t) =>
  List.map(ty => (ctx, ty), tys);

let normalize = ((ctx, ty): t): HTyp.normalized => HTyp.normalize(ctx, ty);

let head_normalize = ((ctx, ty): t): HTyp.head_normalized =>
  HTyp.head_normalize(ctx, ty);

let equivalent = (dty1: t, dty2: t): bool =>
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("dty1", () => sexp_of_t(dty1)),
      ("dty2", () => sexp_of_t(dty2)),
    ],
    ~result_sexp=Sexplib.Std.sexp_of_bool,
    () =>
    HTyp.normalized_equivalent(normalize(dty1), normalize(dty2))
  );

let consistent = (dty1: t, dty2: t): bool =>
  HTyp.normalized_consistent(normalize(dty1), normalize(dty2));

let ground_cases_of = (dty: t): HTyp.ground_cases =>
  HTyp.ground_cases_of(normalize(dty));

let is_hole = (dty: t): bool =>
  HTyp.is_hole(HTyp.of_head_normalized(head_normalize(dty)));

let list = ((tyvars, ty): t): t => (tyvars, HTyp.list(ty));
