open Sexplib.Std;

[@deriving sexp]
type t('a) = list((Var.t, 'a));

let empty: t('a) = [];

let is_empty: t('a) => bool =
  fun
  | [] => true
  | [_, ..._] => false;

let extend = (ctx: t('a), xa: (Var.t, 'a)): t('a) => {
  let (x, _) = xa;
  [xa, ...List.remove_assoc(x, ctx)];
};

let add = (ctx: t('a), x: Var.t, a: 'a): t('a) => [(x, a), ...ctx];

let mem = (ctx: t('a), x: Var.t): bool => List.mem_assoc(x, ctx);

let union = (ctx1: t('a), ctx2: t('a)): t('a) =>
  List.fold_left(extend, ctx2, ctx1);

let lookup = (ctx: t('a), x: Var.t): option('a) => List.assoc_opt(x, ctx);

/* let contains = (ctx: t('a), x: 'a): bool => List.assoc(x, ctx); */

let map = (f: ((Var.t, 'a)) => 'b, xs: t('a)): t('b) =>
  List.map(((x, _) as xa) => (x, f(xa)), xs);

let filter: (((Var.t, 'a)) => bool, t('a)) => t('a) = List.filter;

let length: t('a) => int = List.length;

let to_list: t('a) => list((Var.t, 'a)) = ctx => ctx;
let of_list: list((Var.t, 'a)) => t('a) = ctx => ctx;
