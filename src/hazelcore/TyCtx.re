/** Associates a type variable with its kind */
open TyVar;
// open Sexplib.Std;

module Vars = {
  // [@deriving sexp]
  type binding = (Name.t, Kind.t(HTyp.t));

  // [@deriving sexp]
  type t = list(binding);

  let empty: t = [];

  let bind =
      (
        t: Name.t,
        k: Kind.t(HTyp.t),
        ~increment_singleton: binding => binding,
        tyvars: t,
      )
      : t => {
    let binding = increment_singleton((t, k));
    [binding, ...tyvars |> List.map(increment_singleton)];
  };

  let rec index = (~offset: int=0, x: Name.t, tyvars: t): option(Index.t) =>
    switch (tyvars) {
    | [] => None
    | [(y, _), ...tyvars'] =>
      x == y ? Some(offset) : tyvars' |> index(~offset=offset + 1, x)
    };

  let rec has_index = (i: Index.t, tyvars: t): bool =>
    switch (tyvars) {
    | [] => i == 0
    | [_, ...tyvars'] => i == 0 || tyvars' |> has_index(i - 1)
    };

  let binding = (i: Index.t, tyvars: t): option((Name.t, Kind.t(HTyp.t))) =>
    List.nth_opt(tyvars, i);

  let bound = (name: Name.t, vars: t): bool => vars |> List.mem_assoc(name);

  let kind = (i: Index.t, vars: t): option(Kind.t(HTyp.t)) =>
    List.nth_opt(vars, i) |> Option.map(snd);
};

/** Associates a hole with its kind */
module Holes = {
  include Map.Make(MetaVar);

  type map('a) = t('a);
  type t = map(Kind.t(HTyp.t));

  let kind: (MetaVar.t, t) => option(Kind.t(HTyp.t)) = find_opt;
  // let sexp_of_t = (holes: t): Sexplib.Sexp.t =>
  //   holes
  //   |> bindings
  //   |> Sexplib.Std.sexp_of_list(((u, k)) =>
  //        List([MetaVar.sexp_of_t(u), Kind.sexp_of_t(k)])
  //      );
  // let t_of_sexp = (sexp: Sexplib.Sexp.t): t => {
  //   let binding: Sexplib.Sexp.t => (MetaVar.t, Kind.t(HTyp.t)) =
  //     fun
  //     | Sexplib.Sexp.List([u, k]) => (
  //         MetaVar.t_of_sexp(u),
  //         Kind.t(HTyp.t)_of_sexp(k),
  //       )
  //     | s => raise(Sexplib.Conv_error.tuple_of_size_n_expected("???", 2, s));
  //   sexp |> Sexplib.Std.list_of_sexp(binding) |> List.to_seq |> of_seq;
  // };
};

// [@derivng sexp]
type t = {
  vars: Vars.t,
  holes: Holes.t,
};

type join =
  | GLB
  | LUB;

let empty: t = {vars: Vars.empty, holes: Holes.empty};

let var_index = (name: Name.t, ctx: t): option(Index.t) =>
  ctx.vars |> Vars.index(name);
let var_kind = (i: Index.t, ctx: t): option(Kind.t(HTyp.t)) =>
  ctx.vars |> Vars.kind(i);

let var_bound = (name: Name.t, ctx: t): bool => ctx.vars |> Vars.bound(name);

let bind_var =
    (
      name: TyVar.Name.t,
      k: Kind.t(HTyp.t),
      ~increment_singleton: Vars.binding => Vars.binding,
      ctx: t,
    )
    : t => {
  let vars = ctx.vars |> Vars.bind(name, k, ~increment_singleton);
  {...ctx, vars};
};

let hole_kind = (u: MetaVar.t, ctx: t): option(Kind.t(HTyp.t)) =>
  ctx.holes |> Holes.kind(u);

/** Kind equivalence

Kinds are equivalent if they are equal modulo singletons and singleton types
are all equivalent.
*/

// TODO: (eric) find out if the formalism is right
let rec equivalent_kind =
        (k: Kind.t(HTyp.t), k': Kind.t(HTyp.t), ctx: t): bool =>
  switch (k, k') {
  | (KHole, KHole)
  | (Type, Type) => true
  | (KHole | Type, _) => false
  | (Singleton(Singleton(_, ty), _), Singleton(_, ty'))
  | (Singleton(_, ty), Singleton(Singleton(_, ty'), _))
  | (Singleton(_, ty), Singleton(_, ty')) => ctx |> equivalent(ty, ty')
  | (Singleton(_), _) => false
  }

/** Type equivalence

Types are equivalent if they are closed and of equivalent kinds w.r.t. the
environment, and consistent w.r.t. the empty environment.
*/
and equivalent = (ty: HTyp.t, ty': HTyp.t, ctx: t): bool =>
  switch (ty, ty') {
  /* bound type variables are equivalent to themselves */
  | (TyVar(i, name), TyVar(i', name')) =>
    Index.equal(i, i')
    && TyVar.Name.equal(name, name')
    && Vars.has_index(i, ctx.vars)
  | (TyVar(_), _) => false
  /* type variable holes of known kind are equivalent to themselves */
  | (TyVarHole(reason, u, name), TyVarHole(reason', u', name')) =>
    reason == reason'
    && MetaVar.eq(u, u')
    && TyVar.Name.equal(name, name')
    && Holes.mem(u, ctx.holes)
  | (TyVarHole(_), _) => false
  /* empty type holes of known kind are equivalent to themselves */
  | (Hole(u), Hole(u')) => u == u' && ctx.holes |> Holes.mem(u)
  | (Hole(_), _) => false
  /* base types are equivalent to themselves */
  | (Int | Float | Bool, _) => ty == ty'
  /* composite types are equivalent when they are componentwise equivalent */
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    ctx |> equivalent(ty1, ty1') && ctx |> equivalent(ty2, ty2')
  | (Arrow(_) | Sum(_), _) => false
  | (Prod(tys), Prod(tys')) =>
    List.for_all2((ty, ty') => ctx |> equivalent(ty, ty'), tys, tys')
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => ctx |> equivalent(ty, ty')
  | (List(_), _) => false
  };

/** Type consistency

Types are consistent if they are equivalent modulo holes.
*/
let rec consistent = (ty: HTyp.t, ty': HTyp.t, ctx: t) =>
  switch (ty, ty') {
  /* holes are consistent with anything */
  | (TyVarHole(_), _)
  | (_, TyVarHole(_))
  | (Hole(_), _)
  | (_, Hole(_)) => true
  /* type variables are consistent with equivalent types */
  | (TyVar(_), _)
  | (_, TyVar(_)) => ctx |> equivalent(ty, ty')
  | (Int | Float | Bool, _) => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    ctx |> consistent(ty1, ty1') && ctx |> consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    let check_component = (ty1, ty2) => ctx |> consistent(ty1, ty2);
    ListUtil.for_all2_opt(check_component, tys1, tys2)
    |> Option.value(~default=false);
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => ctx |> consistent(ty, ty')
  | (List(_), _) => false
  };

let inconsistent = (ty1: HTyp.t, ty2: HTyp.t, ctx: t) =>
  !(ctx |> consistent(ty1, ty2));

let rec consistent_all = (types: list(HTyp.t), ctx: t): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    !List.exists(x => ctx |> inconsistent(hd, x), tl)
    || consistent_all(tl, ctx)
  };

let rec join = (j: join, ty1: HTyp.t, ty2: HTyp.t, ctx: t): option(HTyp.t) => {
  switch (ty1, ty2) {
  | (TyVarHole(_, u, _), TyVarHole(_, u', _)) =>
    Some(Hole(MetaVar.join(u, u')))
  | (_, Hole(_))
  | (_, TyVarHole(_)) =>
    switch (j) {
    | GLB => Some(Hole(0))
    | LUB => Some(ty1)
    }
  | (Hole(_), _)
  | (TyVarHole(_), _) =>
    switch (j) {
    | GLB => Some(Hole(0))
    | LUB => Some(ty2)
    }
  | (TyVar(i, _), _) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx.vars |> Vars.binding(i);
    switch (k) {
    | Singleton(_, ty) => ctx |> join(j, ty, ty2)
    | KHole => ctx |> join(j, Hole(0), ty2)
    | Type => failwith("impossible for bounded type variables (currently) 1")
    };
  | (_, TyVar(i, _)) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx.vars |> Vars.binding(i);
    switch (k) {
    | Kind.Singleton(_, ty) => ctx |> join(j, ty1, ty)
    | KHole => ctx |> join(j, ty1, HTyp.Hole(0))
    | Type => failwith("impossible for bounded type variables (currently) 2")
    };
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = ctx |> join(j, ty1, ty1');
    let+ ty2 = ctx |> join(j, ty2, ty2');
    HTyp.Arrow(ty1, ty2);
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = ctx |> join(j, ty1, ty1');
    let+ ty2 = ctx |> join(j, ty2, ty2');
    HTyp.Sum(ty1, ty2);
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt((ty1, ty2) => ctx |> join(j, ty1, ty2), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => HTyp.Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    open OptUtil.Syntax;
    let+ ty = ctx |> join(j, ty, ty');
    HTyp.List(ty);
  | (List(_), _) => None
  };
};

let join_all = (j: join, types: list(HTyp.t), ctx: t): option(HTyp.t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types, ctx)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => ctx |> join(j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};

// open Sexplib.Std;
// [@deriving sexp]
// type t = list((TyId.t, Kind.t(HTyp.t)));
// // TODO: What is identity function
// let of_list = x => x;
// let to_list = x => x;
// let empty = [];
// let extend = (ctx: t, binding: (TyId.t, Kind.t(HTyp.t))): t => {
//   let incr_if_singleton = ((id, k)) =>
//     switch (k) {
//     | Kind.Singleton(k', hty) => (
//         id,
//         Kind.Singleton(k', HTyp.tyvar_debruijn_increment(hty)),
//       )
//     | _ => (id, k)
//     };
//   let rest: list((TyId.t, Kind.t(HTyp.t))) = ctx |> List.map(incr_if_singleton);
//   [incr_if_singleton(binding), ...rest];
// };
// let index_of = (ctx: t, x: TyId.t): option(TyVarIndex.t) => {
//   let rec go = (ctx: t, x: TyId.t, n: int): option(TyVarIndex.t) =>
//     switch (ctx) {
//     | [] => None
//     | [(y, _), ...tl] =>
//       if (TyId.eq(x, y)) {
//         Some(TyVarIndex.of_int(n));
//       } else {
//         go(tl, x, n + 1);
//       }
//     };
//   go(ctx, x, 0);
// };
// let index_of_exn = (ctx: t, x: TyId.t): TyVarIndex.t =>
//   OptUtil.get(() => failwith("identifier not found"), index_of(ctx, x));
// let contains = (ctx: t, x: TyId.t): bool =>
//   Option.is_some(index_of(ctx, x));
// let tyvar_with_idx = TyVarIndex.lookup;
// // TODO: (eric) XXX
// let consistent = (_, _) => false;
