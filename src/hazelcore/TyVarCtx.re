open Sexplib.Std;

[@deriving sexp]
type t = list((TyId.t, Kind.t));

// TODO: What is identity function
let of_list = x => x;
let to_list = x => x;

let empty = [];

let index_of = (ctx: t, x: TyId.t): option(int) => {
  let rec go = (ctx: t, x: TyId.t, n: int): option(int) =>
    switch (ctx) {
    | [] => None
    | [(y, _), ...tl] =>
      if (TyId.eq(x, y)) {
        Some(n);
      } else {
        go(tl, x, n + 1);
      }
    };

  go(ctx, x, 0);
};

let index_of_exn = (ctx: t, x: TyId.t): int =>
  OptUtil.get(() => failwith("identifier not found"), index_of(ctx, x));

let contains = (ctx: t, x: TyId.t): bool =>
  Option.is_some(index_of(ctx, x));

let tyvar_with_idx = HTyp.Index.lookup;
