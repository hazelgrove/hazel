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
  | Var(string)
  | List(t)
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(t, t);

[@deriving (show({with_path: false}), sexp, yojson)]
type equivalence = (t, t)
and constraints = list(equivalence);

let rec typ_to_ityp: Typ.t => t =
  fun
  | Unknown(prov, _) => Unknown(prov)
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(tys) => List(typ_to_ityp(tys))
  | Arrow(t1, t2) => Arrow(typ_to_ityp(t1), typ_to_ityp(t2))
  | Prod([single]) => typ_to_ityp(single)
  | Sum([]) => Unit
  | Sum([sum_entry]) => constructor_binding_to_ityp(sum_entry)
  | Sum([hd, ...tl]) =>
    Sum(constructor_binding_to_ityp(hd), typ_to_ityp(Sum(tl)))
  | Prod([hd_ty, ...tl_tys]) =>
    Prod(typ_to_ityp(hd_ty), typ_to_ityp(Prod(tl_tys)))
  | Prod([]) => Unit
  | Rec(_, ty_body) => typ_to_ityp(ty_body)
  | Var(name) => Var(name)
and constructor_binding_to_ityp = sum_entry => {
  sum_entry |> snd |> Util.OptUtil.get(() => Typ.Prod([])) |> typ_to_ityp;
};

let unwrap_if_prod = (typ: Typ.t): list(Typ.t) => {
  switch (typ) {
  | Prod([hd, ...tl]) => [hd, ...tl]
  | _ => [typ]
  };
};

let rec_type_constraints = (typs: list(Typ.t)): constraints => {
  let is_rec_type = (ty: Typ.t): option(equivalence) =>
    switch (ty) {
    | Rec(var, body) => Some((Var(var), typ_to_ityp(body)))
    | _ => None
    };
  List.filter_map(is_rec_type, typs);
};

let rec ityp_to_typ_ = (prefix, t): Typ.t => {
  let go = ityp_to_typ_(prefix);
  switch (t) {
  | Unknown(prov) => Unknown(prov, false)
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(ity) => List(go(ity))
  | Arrow(t1, t2) => Arrow(go(t1), go(t2))
  | Sum(t1, t2) =>
    let prefix_l = prefix ++ "L";
    let prefix_r = prefix ++ "R";
    Sum([
      (prefix_l, Some(ityp_to_typ_(prefix_l, t1))),
      (prefix_r, Some(ityp_to_typ_(prefix_r, t2))),
    ]);
  | Unit => Prod([])
  | Var(name) => Var(name)
  | Prod(t1, t2) => Prod([go(t1)] @ (t2 |> go |> unwrap_if_prod))
  };
};
let ityp_to_typ = ityp_to_typ_("");

let to_ityp_constraints = (constraints: Typ.constraints): constraints => {
  constraints
  |> List.filter(((t1, t2)) =>
       t1 != Typ.Unknown(NoProvenance, false)
       && t2 != Typ.Unknown(NoProvenance, false)
     )
  |> List.map(((t1, t2)) => (typ_to_ityp(t1), typ_to_ityp(t2)));
};

let rec contains_node = (ty: t): bool =>
  switch (ty) {
  | Unknown(_)
  | Var(_) => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2)
  | Prod(ty1, ty2) => contains_node(ty1) || contains_node(ty2)
  | List(l_ty) => contains_node(l_ty)
  | _ => false
  };
