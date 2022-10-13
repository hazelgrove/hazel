open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Singleton(Typ.t, int)
  | Abstract;

// TODO (poly): truncate ctx and do an actual head_normalize
let head_normalize = (typ: Typ.t): Typ.t => typ;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type t =
//   | Singleton(Typ.t, Ctx.t)
//   | Abstract;

// let rec head_normalize = (~ctx: Ctx.t, typ: Typ.t): Typ.t =>
//   switch (typ) {
//   | Var(x) =>
//     let kind = Ctx.lookup_kind(ctx, x);
//     head_normalize(kind);
//   | List(t) => List(head_normalize(~ctx, t))
//   | Arrow(t1, t2) =>
//     Arrow(head_normalize(~ctx, t1), head_normalize(~ctx, t2))
//   | Prod(ts) => Prod(List.map(head_normalize(~ctx), ts))
//   | Sum(t1, t2) => Sum(head_normalize(~ctx, t1), head_normalize(~ctx, t2))
//   | _ => typ
//   };
