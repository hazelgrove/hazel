open Sexplib.Std;
exception TypeVarUnsupported;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Unknown(Typ.type_provenance)
  | Unit
  | Int
  | Float
  | Bool
  | String
  | List(t)
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(t, t);

[@deriving (show({with_path: false}), sexp, yojson)]
type equivalence = (t, t)
and constraints = list(equivalence);

// HACK:
//    In order to ensure difference hole provenenances with the same
//    id chart to the same set of results, convert synswitch provs to internal
//    Do not change TypeHole, as we will filter on that condition later.
let rec prov_to_iprov: Typ.type_provenance => Typ.type_provenance =
  fun
  | SynSwitch(u) => Internal(u)
  | Inference(mprov, prov) => Inference(mprov, prov_to_iprov(prov))
  | _ as prov => prov;

let rec typ_to_ityp: Typ.t => t =
  fun
  | Unknown(prov) => Unknown(prov_to_iprov(prov))
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(tys) => List(typ_to_ityp(tys))
  | Arrow(t1, t2) => Arrow(typ_to_ityp(t1), typ_to_ityp(t2))
  | Sum(t1, t2) => Sum(typ_to_ityp(t1), typ_to_ityp(t2))
  | Prod([single]) => typ_to_ityp(single)
  | Prod([hd_ty, ...tl_tys]) =>
    Prod(typ_to_ityp(hd_ty), typ_to_ityp(Prod(tl_tys)))
  | Prod([]) => Unit
  | Var(_) => raise(TypeVarUnsupported);

let rec ityp_to_typ: t => Typ.t =
  fun
  | Unknown(prov) => Unknown(prov)
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(ity) => List(ityp_to_typ(ity))
  | Arrow(t1, t2) => Arrow(ityp_to_typ(t1), ityp_to_typ(t2))
  | Sum(t1, t2) => Sum(ityp_to_typ(t1), ityp_to_typ(t2))
  | Unit => Prod([])
  | Prod(t1, t2) => Prod([ityp_to_typ(t1), ityp_to_typ(t2)]);

let to_ityp_constraints = (constraints: Typ.constraints): constraints => {
  constraints
  |> List.filter(((t1, t2)) =>
       t1 != Typ.Unknown(Anonymous) && t2 != Typ.Unknown(Anonymous)
     )
  |> List.map(((t1, t2)) => (typ_to_ityp(t1), typ_to_ityp(t2)));
};

let rec contains_hole = (ty: t): bool =>
  switch (ty) {
  | Unknown(_) => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2)
  | Prod(ty1, ty2) => contains_hole(ty1) || contains_hole(ty2)
  | _ => false
  };
