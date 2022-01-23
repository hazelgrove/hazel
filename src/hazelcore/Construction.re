// // Construction.re encompasses Kinds and Types and Type constructors
// open Kind;
// /// k1 is a consistent_subkind of k2
// let rec consistent_subkind = (ctx, k1, k2) =>
//   switch (k1, k2) {
//   | (KHole, _)
//   | (_, KHole) => true
//   | (Singleton(_), Type) => true
//   | (k1, k2) when kequiv(ctx, k1, k2) => true
//   | (_, _) => false
//   }
// // k1 is equivalent to k2
// and kequiv = (ctx, k1, k2) =>
//   switch (k1, k2) {
//   | (KHole, KHole)
//   | (Type, Type) => true
//   // S_{S_k(c)}(c') === S_k(c)
//   | (Singleton(Singleton(_k, ty1), _), Singleton(_, ty2))
//       when kcequiv(ctx, ty1, ty2) =>
//     true
//   | (Singleton(_, ty1), Singleton(_, ty2)) when kcequiv(ctx, ty1, ty2) =>
//     true
//   | (KHole | Type | Singleton(_), _) => false
//   }
// // ty1 is equivalent to ty2 and its kind is k
// and kcequiv = (ctx, ty1, ty2): bool => {
//   switch (ty1, ty2) {
//   | (TyVar(idx1, _), TyVar(idx2, _)) when idx1 == idx2 => true
//   | (TyVarHole(_, id1), TyVarHole(_, id2)) => TyId.eq(id1, id2)
//   // TODO: Make these not equivalent when we add identifiers
//   | (Hole, Hole)
//   | (Int, Int)
//   | (Bool, Bool)
//   | (Float, Float) => true
//   | (Arrow(ty1, ty3), Arrow(ty2, ty4))
//   | (Sum(ty1, ty3), Sum(ty2, ty4)) =>
//     kcequiv(ctx, ty1, ty3) && kcequiv(ctx, ty2, ty4)
//   | (Prod(ts), Prod(ts')) when List.length(ts) == List.length(ts') =>
//     List.combine(ts, ts') |> List.for_all(((x, y)) => kcequiv(ctx, x, y))
//   | (List(ty1), List(ty2)) => kcequiv(ctx, ty1, ty2)
//   | (
//       Hole | Int | Bool | Float | Arrow(_, _) | Sum(_, _) | Prod(_) | List(_) |
//       TyVar(_),
//       _,
//     )
//   | (
//       _,
//       Hole | Int | Bool | Float | Arrow(_, _) | Sum(_, _) | Prod(_) | List(_) |
//       TyVar(_),
//     ) =>
//     false
//   };
// };
// module HTyp = {
//   open HTyp;
//   type t = HTyp.t;
//   type join = HTyp.join;
//   // TODO: (eric) XXX redo kinded type equivalence
//   // let equiv = kcequiv;
//   let equiv = (_, _) => false;
//   let rec consistent = (ctx: Contexts.t, x, y) =>
//     switch (x, y) {
//     | (Hole, _)
//     | (_, Hole) => true
//     // TODO: (eric) implement type variable consistency check
//     | (TyVar(_), _) => failwith(__LOC__ ++ ": XXX")
//     | (TyVarHole(_), _) => failwith(__LOC__ ++ ": XXX")
//     // | (TyVarHole(_), _)
//     // | (_, TyVarHole(_)) => true
//     // | (TyVar(i, _), _) =>
//     //   let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), i);
//     //   switch (k) {
//     //   | Kind.Singleton(_, ty) => consistent(ctx, ty, y)
//     //   | KHole => true
//     //   | Type =>
//     //     failwith("impossible for bounded type variables (currently) 5")
//     //   };
//     // | (_, TyVar(i, _)) =>
//     //   let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), i);
//     //   switch (k) {
//     //   | Kind.Singleton(_, ty) => consistent(ctx, x, ty)
//     //   | KHole => true
//     //   | Type =>
//     //     failwith("impossible for bounded type variables (currently) 6")
//     //   };
//     | (Int, Int) => true
//     | (Int, _) => false
//     | (Float, Float) => true
//     | (Float, _) => false
//     | (Bool, Bool) => true
//     | (Bool, _) => false
//     | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
//     | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
//       consistent(ctx, ty1, ty1') && consistent(ctx, ty2, ty2')
//     | (Arrow(_, _), _) => false
//     | (Sum(_, _), _) => false
//     | (Prod(tys1), Prod(tys2)) =>
//       ListUtil.for_all2_opt(consistent(ctx), tys1, tys2)
//       |> Option.value(~default=false)
//     | (Prod(_), _) => false
//     | (List(ty), List(ty')) => consistent(ctx, ty, ty')
//     | (List(_), _) => false
//     };
//   let inconsistent = (ctx, ty1, ty2) => !consistent(ctx, ty1, ty2);
//   let rec consistent_all = (ctx, types: list(t)): bool =>
//     switch (types) {
//     | [] => true
//     | [hd, ...tl] =>
//       if (List.exists(inconsistent(ctx, hd), tl)) {
//         false;
//       } else {
//         consistent_all(ctx, tl);
//       }
//     };
//   let rec join = (ctx: Contexts.t, j:join, ty1:HTyp.t, ty2) =>
//     switch (ty1, ty2) {
//     | (TyVarHole(_, _), TyVarHole(_, _)) => Some(Hole)
//     | (_, Hole)
//     | (_, TyVarHole(_)) =>
//       switch (j) {
//       | GLB => Some(Hole)
//       | LUB => Some(ty1)
//       }
//     | (Hole, _)
//     | (TyVarHole(_), _) =>
//       switch (j) {
//       | GLB => Some(Hole)
//       | LUB => Some(ty2)
//       }
//     | (TyVar(i, _), _) =>
//       let (_, k) = TyVar.Ctx.tyvar_with_idx(Contexts.tyvars(ctx), i);
//       switch (k) {
//       | Kind.Singleton(_, ty) => join(ctx, j, ty, ty2)
//       | KHole => join(ctx, j, Hole, ty2)
//       | Type =>
//         failwith("impossible for bounded type variables (currently) 1")
//       };
//     | (_, TyVar(i, _)) =>
//       let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), i);
//       switch (k) {
//       | Kind.Singleton(_, ty) => join(ctx, j, ty1, ty)
//       | KHole => join(ctx, j, ty1, Hole)
//       | Type =>
//         failwith("impossible for bounded type variables (currently) 2")
//       };
//     | (Int, Int) => Some(ty1)
//     | (Int, _) => None
//     | (Float, Float) => Some(ty1)
//     | (Float, _) => None
//     | (Bool, Bool) => Some(ty1)
//     | (Bool, _) => None
//     | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
//       switch (join(ctx, j, ty1, ty1'), join(ctx, j, ty2, ty2')) {
//       | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
//       | _ => None
//       }
//     | (Arrow(_), _) => None
//     | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
//       switch (join(ctx, j, ty1, ty1'), join(ctx, j, ty2, ty2')) {
//       | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
//       | _ => None
//       }
//     | (Sum(_), _) => None
//     | (Prod(tys1), Prod(tys2)) =>
//       ListUtil.map2_opt(join(ctx, j), tys1, tys2)
//       |> Option.map(OptUtil.sequence)
//       |> Option.join
//       |> Option.map(joined_types => Prod(joined_types))
//     | (Prod(_), _) => None
//     | (List(ty), List(ty')) =>
//       switch (join(ctx, j, ty, ty')) {
//       | Some(ty) => Some(List(ty))
//       | None => None
//       }
//     | (List(_), _) => None
//     };
//   let join_all = (ctx, j: join, types: list(t)): option(t) => {
//     switch (types) {
//     | [] => None
//     | [hd] => Some(hd)
//     | [hd, ...tl] =>
//       if (!consistent_all(ctx, types)) {
//         None;
//       } else {
//         List.fold_left(
//           (common_opt, ty) =>
//             switch (common_opt) {
//             | None => None
//             | Some(common_ty) => join(ctx, j, common_ty, ty)
//             },
//           Some(hd),
//           tl,
//         );
//       }
//     };
//   };
// };
