/** Associates a type variable with its kind */
open TyVar;

module HTyp = HTypCore;
module Kind = Kind;

module Vars = {
  open Sexplib.Std;

  [@deriving sexp]
  type binding = (Name.t, Kind.t);

  [@deriving sexp]
  type t = list(binding);

  let empty: t = [];

  let bind = (name: Name.t, k: Kind.t, tyvars: t): t => {
    let increment_singleton: binding => binding =
      fun
      | (name, Singleton(k', ty)) => {
          let k = Kind.Singleton(k', HTyp.increment_indices(ty));
          (name, k);
        }
      | binding => binding;
    let binding = increment_singleton((name, k));
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

  let binding = (i: Index.t, tyvars: t): option((Name.t, Kind.t)) =>
    List.nth_opt(tyvars, i);

  let bound = (name: Name.t, vars: t): bool => vars |> List.mem_assoc(name);

  let kind = (i: Index.t, vars: t): option(Kind.t) =>
    List.nth_opt(vars, i) |> Option.map(snd);
};

/** Associates a hole with its kind */
module Holes = {
  include Map.Make(MetaVar);

  type map('a) = t('a);
  type t = map(Kind.t);

  let kind: (MetaVar.t, t) => option(Kind.t) = find_opt;

  let sexp_of_t = (holes: t): Sexplib.Sexp.t =>
    holes
    |> bindings
    |> Sexplib.Std.sexp_of_list(((u, k)) =>
         List([MetaVar.sexp_of_t(u), Kind.sexp_of_t(k)])
       );

  let t_of_sexp = (sexp: Sexplib.Sexp.t): t => {
    let binding: Sexplib.Sexp.t => (MetaVar.t, Kind.t) =
      fun
      | Sexplib.Sexp.List([u, k]) => (
          MetaVar.t_of_sexp(u),
          Kind.t_of_sexp(k),
        )
      | s => raise(Sexplib.Conv_error.tuple_of_size_n_expected("???", 2, s));
    sexp |> Sexplib.Std.list_of_sexp(binding) |> List.to_seq |> of_seq;
  };
};

[@deriving sexp]
type t = {
  vars: Vars.t,
  holes: Holes.t,
};

let empty: t = {vars: Vars.empty, holes: Holes.empty};

let bound_var = (name: Name.t, ctx: t): bool => ctx.vars |> Vars.bound(name);
let has_var_index = (i: Index.t, ctx: t): bool =>
  ctx.vars |> Vars.has_index(i);

let var_index = (name: Name.t, ctx: t): option(Index.t) =>
  ctx.vars |> Vars.index(name);
let var_kind = (i: Index.t, ctx: t): option(Kind.t) =>
  ctx.vars |> Vars.kind(i);

let var_binding = (i: Index.t, ctx: t): option(Vars.binding) =>
  ctx.vars |> Vars.binding(i);

let bind_var = (name: TyVar.Name.t, k: Kind.t, ctx: t): t => {
  let vars = ctx.vars |> Vars.bind(name, k);
  {...ctx, vars};
};

let has_hole = (u: MetaVar.t, ctx: t): bool => ctx.holes |> Holes.mem(u);

let hole_kind = (u: MetaVar.t, ctx: t): option(Kind.t) =>
  ctx.holes |> Holes.kind(u);

// open Sexplib.Std;
// [@deriving sexp];
// type t = list((TyId.t, Kind.t));
// // TODO: What is identity function
// let of_list = x => x;
// let to_list = x => x;
// let empty = [];
// let extend = (ctx: t, binding: (TyId.t, Kind.t)): t => {
//   let incr_if_singleton = ((id, k)) =>
//     switch (k) {
//     | Kind.Singleton(k', hty) => (
//         id,
//         Kind.Singleton(k', HTyp.tyvar_debruijn_increment(hty)),
//       )
//     | _ => (id, k)
//     };
//   let rest: list((TyId.t, Kind.t)) = ctx |> List.map(incr_if_singleton);
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
