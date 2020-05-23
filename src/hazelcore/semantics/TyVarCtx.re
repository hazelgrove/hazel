open Sexplib.Std;

[@deriving sexp]
type t = list((TyId.t, Kind.t));

let empty = [];

let rec _index_of = (ctx: t, x: TyId.t, n: int): int =>
  switch (ctx) {
  | [] => failwith("identifier not found")
  | [(y, _), ...tl] => 
    if (TyId.eq(x, y)) {
      n
    } else {
      _index_of(tl, x, n + 1)
    }
  };

let index_of = (ctx: t, x: TyId.t): int => _index_of(ctx, x, 0);
