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

let to_htyp: t => HTyp.t;

let from_htyp: HTyp.t => t;

let htyp_prod_to_inf_typ: list(HTyp.t) => t;
