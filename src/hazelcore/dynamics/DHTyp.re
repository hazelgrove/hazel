[@deriving sexp]
type t = (HTyp.t, TyCtx.t);

let lift = (ctx: TyCtx.t, ty: HTyp.t): t => (ty, ctx);
let type_ = ((ty, _): t): HTyp.t => ty;
let context = ((_, ctx): t): TyCtx.t => ctx;

// /** Type equivalence

// Types are equivalent if they are closed and of equivalent kinds w.r.t their
// respective environments, and equal (modulo indices).
// */
// let rec equivalent = ((ty, ctx): t, (ty', ctx'): t): bool =>
//   HTyp.equal(ty, ty')
//   && (
//     switch (ty, ty') {
//     /* bound type variables are equivalent to themselves */
//     | (TyVar(i, _), TyVar(i', _)) =>
//       {
//         open OptUtil.Syntax;
//         let* k1 = ctx |> TyCtx.var_kind(i);
//         let+ k1' = ctx' |> TyCtx.var_kind(i');
//         equivalent_kind(lift_kind(ctx, k1), lift_kind(ctx', k1'));
//       }
//       |> Option.value(~default=false)
//     | (TyVar(_), _) => false
//     /* type variable holes of known kinds are equivalent to themselves */
//     | (TyVarHole(_, u, _), TyVarHole(_, u', _)) =>
//       switch (TyCtx.(hole_kind(u, ctx), hole_kind(u', ctx'))) {
//       | (Some(k), Some(k')) =>
//         equivalent_kind(lift_kind(ctx, k), lift_kind(ctx', k'))
//       | (_, _) => false
//       }
//     | (TyVarHole(_), _) => false
//     /* empty type holes of known kind are equivalent to themselves */
//     | (Hole, Hole) =>
//       {
//         open OptUtil.Syntax;
//         let* k = ctx |> TyCtx.hole_kind(u);
//         let+ k' = ctx' |> TyCtx.hole_kind(u');
//         equivalent_kind(lift_kind(ctx, k), lift_kind(ctx', k'));
//       }
//       |> Option.value(~default=false)
//     | (Hole, _) => false
//     /* base types are equivalent to themselves */
//     | (Int | Float | Bool, _) => true
//     /* composite types are equivalent when they are componentwise equivalent */
//     | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
//     | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
//       equivalent((ty1, ctx), (ty1', ctx'))
//       && equivalent((ty2, ctx), (ty2', ctx'))
//     | (Arrow(_) | Sum(_), _) => false
//     | (Prod(tys), Prod(tys')) =>
//       let check_element = (ty, ty') => equivalent((ty, ctx), (ty', ctx'));
//       List.for_all2(check_element, tys, tys');
//     | (Prod(_), _) => false
//     | (List(ty), List(ty')) => equivalent((ty, ctx), (ty', ctx'))
//     | (List(_), _) => false
//     }
//   )

// and equivalent_kind = (k: Kind.t, k': Kind.t): bool =>
//   switch (k, k') {
//   | (KHole, KHole)
//   | (Type, Type) => true
//   | (KHole | Type, _) => false
//   | (Singleton(k1, dty), Singleton(k1', dty')) =>
//     equivalent(dty, dty')
//     || (
//       switch (k1, k1') {
//       | (Singleton(_, dty), _) => equivalent(dty, dty')
//       | (_, Singleton(_, dty')) => equivalent(dty, dty')
//       | (_, _) => false
//       }
//     )
//   | (Singleton(_), _) => false
//   };
