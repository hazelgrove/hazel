open Sexplib.Std;

[@deriving sexp]
type t('a) = list((TyVar.t, 'a));

let to_list: t('a) => list((TyVar.t, 'a)) = ctx => ctx;
let of_list: list((TyVar.t, 'a)) => t('a) = ctx => ctx;

let map = (f: ((TyVar.t, 'a)) => 'b, xs: t('a)): t('b) =>
  List.map(((x, _) as xa) => (x, f(xa)), xs);
