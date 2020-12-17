open Sexplib.Std;

/* pattern types with holes */
[@deriving sexp]
type t =
  | Hole
  | Unspecified
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

let rec pTyp_to_hTyp = (ty: t): HTyp.t => {
  switch (ty) {
  | Hole
  | Unspecified => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(t1, t2) => Arrow(pTyp_to_hTyp(t1), pTyp_to_hTyp(t2))
  | Sum(t1, t2) => Sum(pTyp_to_hTyp(t1), pTyp_to_hTyp(t2))
  | Prod(ts) => Prod(List.map(pTyp_to_hTyp, ts))
  | List(t) => List(pTyp_to_hTyp(t))
  };
};

let rec hTyp_to_pTyp = (ty: HTyp.t): t => {
  switch (ty) {
  | Hole => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(t1, t2) => Arrow(hTyp_to_pTyp(t1), hTyp_to_pTyp(t2))
  | Sum(t1, t2) => Sum(hTyp_to_pTyp(t1), hTyp_to_pTyp(t2))
  | Prod(ts) => Prod(List.map(hTyp_to_pTyp, ts))
  | List(t) => List(hTyp_to_pTyp(t))
  };
};

let consistent = (x, y) => {
  let x = pTyp_to_hTyp(x);
  let y = pTyp_to_hTyp(y);
  HTyp.consistent(x, y);
};

let inconsistent = (ty1, ty2) => !consistent(ty1, ty2);

let rec consistent_all = (types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    if (List.exists(inconsistent(hd), tl)) {
      false;
    } else {
      consistent_all(tl);
    }
  };

let rec join = (exp_ty: HTyp.t, pat_ty: t): option(HTyp.t) =>
  switch (exp_ty, pat_ty) {
  | (ty, Unspecified) => Some(ty)
  | (_, Hole) => Some(Hole)
  | (Hole, _) => Some(pTyp_to_hTyp(pat_ty))
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    switch (join(ty1, ty1'), join(ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    switch (join(ty1, ty1'), join(ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_), _) => None
  | (Prod(h_tys), Prod(p_tys)) =>
    ListUtil.map2_opt(join, h_tys, p_tys)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => HTyp.Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    switch (join(ty, ty')) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

let join_all = (types: list(t)): option(HTyp.t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(pTyp_to_hTyp(hd))
  | [hd, ...tl] =>
    if (!consistent_all(types)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => join(common_ty, ty)
          },
        Some(pTyp_to_hTyp(hd)),
        tl,
      );
    }
  };
};
