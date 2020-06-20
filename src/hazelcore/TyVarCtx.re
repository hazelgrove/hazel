open Sexplib.Std;

[@deriving sexp]
type t = list((TyId.t, Kind.t));

let empty = [];

let rec _index_of = (ctx: t, x: TyId.t, n: int): option(int) =>
  switch (ctx) {
  | [] => None
  | [(y, _), ...tl] =>
    if (TyId.eq(x, y)) {
      Some(n);
    } else {
      _index_of(tl, x, n + 1);
    }
  };

let index_of = (ctx: t, x: TyId.t): option(int) => _index_of(ctx, x, 0);

let index_of_exn = (ctx: t, x: TyId.t): int =>
  OptUtil.get(() => failwith("identifier not found"), index_of(ctx, x));

let contains = (ctx: t, x: TyId.t): bool =>
  Option.is_some(index_of(ctx, x));

let tyvar_with_idx = (ctx: t, idx: HTyp.idx): (TyId.t, Kind.t) =>
  List.nth(ctx, idx);
