open Sexplib.Std;

[@deriving sexp]
type t =
  | Unknown(HTyp.unknown_type_provenance)
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(t, t)
  | List(t);

type inf_constraint = (t, t);

let rec to_htyp: t => HTyp.t =
  (ityp: t) =>
    switch (ityp) {
    | Unknown(prov) => Unknown(prov)
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(ityp_l, ityp_r) => Arrow(to_htyp(ityp_l), to_htyp(ityp_r))
    | Sum(ityp_l, ityp_r) => Sum(to_htyp(ityp_l), to_htyp(ityp_r))
    | Prod(ityp_l, ityp_r) => Prod([to_htyp(ityp_l), to_htyp(ityp_r)])
    | List(ityp) => List(ityp |> to_htyp)
    };

let rec from_htyp: HTyp.t => t =
  (htyp: HTyp.t) =>
    switch (htyp) {
    | Unknown(prov) => Unknown(prov)
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(ityp_l, ityp_r) => Arrow(from_htyp(ityp_l), from_htyp(ityp_r))
    | Sum(ityp_l, ityp_r) => Sum(from_htyp(ityp_l), from_htyp(ityp_r))
    | Prod(prod_ls) => htyp_prod_to_inf_typ(prod_ls)
    | List(ityp) => List(from_htyp(ityp))
    }
and htyp_prod_to_inf_typ: list(HTyp.t) => t =
  (ls: list(HTyp.t)) =>
    switch (ls) {
    | []
    | [_] => failwith("Impossible, Prod types should come in pairs.")
    | [first, second] => Prod(first |> from_htyp, second |> from_htyp)
    | [hd, ...tl] => Prod(hd |> from_htyp, htyp_prod_to_inf_typ(tl))
    };
