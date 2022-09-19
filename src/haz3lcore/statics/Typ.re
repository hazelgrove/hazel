include TypBase;

module HeadNormalize = {
  let rec head_normalize = (tctx: TypCtx.t, t: TypBase.t): TypBase.hn => {
    switch (t) {
    | Unknown(tp) => Unknown(tp)
    | TVar(v) =>
      switch (VarMap.lookup(tctx, v)) {
      | Some({item: k, _}) =>
        switch (k) {
        | Singleton(t) => head_normalize(tctx, t)
        | _ => Unknown(Internal)
        }
      | None => Unknown(Internal)
      }
    | Int => Int
    | Float => Float
    | Bool => Bool
    | List(t) => List(head_normalize(tctx, t))
    | Arrow(t1, t2) =>
      Arrow(head_normalize(tctx, t1), head_normalize(tctx, t2))
    | Prod(ts) => Prod(List.map(head_normalize(tctx), ts))
    };
  };

  let rec to_typ: TypBase.hn => TypBase.t =
    fun
    | Unknown(tp) => Unknown(tp)
    | TVar(v) => TVar(v)
    | Int => Int
    | Float => Float
    | Bool => Bool
    | List(t) => List(to_typ(t))
    | Arrow(t1, t2) => Arrow(to_typ(t1), to_typ(t2))
    | Prod(ts) => Prod(List.map(to_typ, ts));
};

/** Join **/

/* How type provenance information should be collated when
   joining unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let join_type_provenance =
    (p1: type_provenance, p2: type_provenance): type_provenance =>
  switch (p1, p2) {
  | (TypeHole, TypeHole | Internal | SynSwitch)
  | (Internal | SynSwitch, TypeHole) => TypeHole
  | (Internal, Internal | SynSwitch)
  | (SynSwitch, Internal) => Internal
  | (SynSwitch, SynSwitch) => SynSwitch
  };

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown */
let rec join_hn = (tctx: TypCtx.t, ty1: hn, ty2: hn): option(hn) =>
  switch (ty1, ty2) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
  // TODO: Fix the behavior for abstract types
  | (TVar(_), ty)
  | (ty, TVar(_)) => Some(ty)
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join_hn(tctx, ty1_1, ty2_1), join_hn(tctx, ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      switch (List.map2(join_hn(tctx), tys1, tys2) |> Util.OptUtil.sequence) {
      | None => None
      | Some(tys) => Some(Prod(tys))
      };
    }
  | (Prod(_), _) => None
  | (List(ty_1), List(ty_2)) =>
    switch (join_hn(tctx, ty_1, ty_2)) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

let join = (tctx: TypCtx.t, ty1: t, ty2: t): option(t) => {
  let ty1 = HeadNormalize.head_normalize(tctx, ty1);
  let ty2 = HeadNormalize.head_normalize(tctx, ty2);
  switch (join_hn(tctx, ty1, ty2)) {
  | Some(ty) => Some(HeadNormalize.to_typ(ty))
  | None => None
  };
};

let join_all = (tctx: TypCtx.t, ts: list(t)): option(t) => {
  let res =
    List.fold_left(
      (acc, ty) => {
        let ty = HeadNormalize.head_normalize(tctx, ty);
        Util.OptUtil.and_then(join_hn(tctx, ty), acc);
      },
      Some(Unknown(Internal)),
      ts,
    );

  switch (res) {
  | Some(ty) => Some(HeadNormalize.to_typ(ty))
  | None => None
  };
};

let join_or_fst = (tctx: TypCtx.t, ty: t, ty': t): t =>
  switch (join(tctx, ty, ty')) {
  | None => ty
  | Some(ty) => ty
  };

let t_of_self = (tctx: TypCtx.t) =>
  fun
  | Just(t) => t
  | Joined(wrap, ss) =>
    switch (ss |> List.map(s => s.ty) |> join_all(tctx)) {
    | None => Unknown(Internal)
    | Some(t) => wrap(t)
    }
  | Multi
  | Free => Unknown(Internal);
