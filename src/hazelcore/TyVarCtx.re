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
  | None => None
  | Some(n) => Some(n)
  };

let index_of_exn = (ctx: t, x: TyId.t): int =>
  OptUtil.get(() => failwith("identifier not found"), index_of(ctx, x));

let contains = (ctx: t, x: TyId.t): bool =>
  Option.is_some(index_of(ctx, x));

let tyvar_with_idx = (ctx: t, idx: HTyp.idx): (TyId.t, Kind.t) =>
  List.nth(ctx, idx);

let rec incr_idx = (hty: HTyp.t): HTyp.t =>
  switch (hty) {
  | TyVar(idx, id) => TyVar(idx + 1, id)
  | TyVarHole(_)
  | Hole
  | Int
  | Float
  | Bool => hty
  | Arrow(t1, t2) => Arrow(incr_idx(t1), incr_idx(t2))
  | Sum(t1, t2) => Sum(incr_idx(t1), incr_idx(t2))
  | List(t) => List(incr_idx(t))
  | Prod(lst) => Prod(List.map(incr_idx, lst))
  };

let extend = (ctx: t, binding: (TyId.t, Kind.t)) => {
  let helper = ((id: TyId.t, k: Kind.t)) =>
    switch (k) {
    | Singleton(hty) => (id, Kind.Singleton(incr_idx(hty)))
    | _ => (id, k)
    };
  let new_ctx = [binding, ...ctx];
  List.map(helper, new_ctx);
};

let rec print = (ctx: t) =>
  switch (ctx) {
  | [] => 0
  | [(y, k), ...tl] =>
    let _ = print_endline(y);
    switch (k) {
    | Type => print_endline("kind")
    | KHole => print_endline("khole")
    | Singleton(TyVar(_, x)) => print_endline("HTyp: " ++ x)
    | Singleton(TyVarHole(_, x)) => print_endline("HTyp: " ++ x)
    | Singleton(Hole) => print_endline("HTyp: " ++ "hole")
    | Singleton(Int) => print_endline("HTyp: " ++ "int")
    | Singleton(Float) => print_endline("HTyp: " ++ "float")
    | Singleton(Bool) => print_endline("HTyp: " ++ "boool")
    | Singleton(_) => print_endline("HTyp: " ++ "others")
    };
    print(tl);
  };
