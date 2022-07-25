open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type type_provenance =
  | ModeSwitch
  | Internal;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Unknown(type_provenance)
  | Int
  | Bool
  | Arrow(t, t)
  | Prod(t, t);

[@deriving (show({with_path: false}), sexp, yojson)]
type self =
  // is this just a list??
  | Just(t)
  | Joined(list(t))
  | Free;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  //| FunPos?
  | Syn
  | Ana(t);

let rec join = (ty1: t, ty2: t): option(t) =>
  switch (ty1, ty2) {
  | (Unknown(Internal), Unknown(_))
  | (Unknown(_), Unknown(Internal)) => Some(Unknown(Internal))
  | (Unknown(_), a)
  | (a, Unknown(_)) => Some(a)
  | (Int, Int) => Some(Int)
  | (Bool, Bool) => Some(Bool)
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join(ty1_1, ty2_1), join(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Prod(ty1_1, ty1_2), Prod(ty2_1, ty2_2)) =>
    switch (join(ty1_1, ty2_1), join(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Prod(ty1, ty2))
    | _ => None
    }
  | _ => None
  };

let join_all: list(t) => option(t) =
  List.fold_left(
    (acc, ty) => Util.OptUtil.and_then(join(ty), acc),
    Some(Unknown(Internal)),
  );

let join_or_fst = (ty: t, ty': t): t =>
  switch (join(ty, ty')) {
  | None => ty
  | Some(ty) => ty
  };

let matched_arrow: t => (t, t) =
  fun
  | Arrow(ty_in, ty_out) => (ty_in, ty_out)
  | Unknown(prov) => (Unknown(prov), Unknown(prov))
  | _ => (Unknown(Internal), Unknown(Internal));

let matched_prod: t => (t, t) =
  fun
  | Prod(ty_in, ty_out) => (ty_in, ty_out)
  | Unknown(prov) => (Unknown(prov), Unknown(prov))
  | _ => (Unknown(Internal), Unknown(Internal));

let matched_arrow_mode: mode => (mode, mode) =
  fun
  | Syn => (Syn, Syn)
  | Ana(ty) => {
      let (ty_in, ty_out) = matched_arrow(ty);
      (Ana(ty_in), Ana(ty_out));
    };

let matched_prod_mode: mode => (mode, mode) =
  fun
  | Syn => (Syn, Syn)
  | Ana(ty) => {
      let (ty_l, ty_r) = matched_prod(ty);
      (Ana(ty_l), Ana(ty_r));
    };

// TODO: clarify
let of_mode: mode => t =
  fun
  | Syn => Unknown(Internal) //TODO
  | Ana(ty) => ty;

// TODO: clarify; compare to Statics.error_status
let of_self: self => t =
  fun
  | Free => Unknown(Internal) //TODO
  | Just(t) => t
  | Joined(tys) =>
    switch (join_all(tys)) {
    | None => Unknown(Internal) //TODO
    | Some(t) => t
    };

// What the type would be as if after hole-fixing
let reconcile = (mode: mode, self: self): t =>
  switch (mode) {
  | Syn => of_self(self)
  | Ana(ty) =>
    switch (join(ty, of_self(self))) {
    | None => Unknown(Internal) //TODO
    | Some(ty) => ty
    }
  };
