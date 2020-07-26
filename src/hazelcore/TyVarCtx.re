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

let index_of = (ctx: t, x: TyId.t): option(int) =>
  switch (_index_of(ctx, x, 0)) {
  | None =>
    let _ = print_endline("not found");
    None;
  | Some(n) =>
    let _ = print_endline(string_of_int(n));
    Some(n);
  };

let index_of_exn = (ctx: t, x: TyId.t): int =>
  OptUtil.get(() => failwith("identifier not found"), index_of(ctx, x));

let contains = (ctx: t, x: TyId.t): bool =>
  Option.is_some(index_of(ctx, x));

let tyvar_with_idx = (ctx: t, idx: HTyp.idx): (TyId.t, Kind.t) =>
  List.nth(ctx, idx);

let extend = (ctx: t, binding: (TyId.t, Kind.t)) => [binding, ...ctx];

let rec print = (ctx: t) =>
  switch (ctx) {
  | [] => 0
  | [(y, _), ...tl] =>
    let _ = print_endline(y);
    print(tl);
  };
