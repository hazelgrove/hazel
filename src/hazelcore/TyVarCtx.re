open Sexplib.Std;

[@deriving sexp]
type t = list((TyId.t, Kind.t));

// TODO: What is identity function
let of_list = x => x;
let to_list = x => x;

let empty = [];

// TODO: Handle singeltons properly
let extend = (ctx: t, binding: (TyId.t, Kind.t)): t => {
  [binding, ...ctx];
};

let index_of = (ctx: t, x: TyId.t): option(HTyp.Index.t) => {
  let rec go = (ctx: t, x: TyId.t, n: int): option(HTyp.Index.t) =>
    switch (ctx) {
    | [] => None
    | [(y, _), ...tl] =>
      if (TyId.eq(x, y)) {
        Some(HTyp.Index.of_int(n));
      } else {
        go(tl, x, n + 1);
      }
    };

  go(ctx, x, 0);
};

let index_of_exn = (ctx: t, x: TyId.t): HTyp.Index.t =>
  OptUtil.get(() => failwith("identifier not found"), index_of(ctx, x));

let contains = (ctx: t, x: TyId.t): bool =>
  Option.is_some(index_of(ctx, x));

let tyvar_with_idx = HTyp.Index.lookup;
