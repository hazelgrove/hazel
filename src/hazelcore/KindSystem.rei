/** Higher-Kinded Types with Holes */

module rec HTyp: {
  [@deriving sexp]
  type t('idx) =
    | Hole
    | Int
    | Float
    | Bool
    | Arrow(t('idx), t('idx))
    | Sum(t('idx), t('idx))
    | Prod(list(t('idx)))
    | List(t('idx))
    | TyVar(Index.t('idx), TyVar.t)
    | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t);

  let equal: (t('idx), t('idx)) => bool;

  [@deriving sexp]
  type rel = t(Index.relative);

  [@deriving sexp]
  type abs = t(Index.absolute);

  let to_rel: (~offset: int=?, abs) => rel;
  let to_abs: (~offset: int=?, rel) => abs;
}

and Kind: {
  [@deriving sexp]
  type t('idx) =
    | Hole
    | Type
    | S(HTyp.t('idx));

  let equal: (t('idx), t('idx)) => bool;

  [@deriving sexp]
  type rel = t(Index.relative);

  [@deriving sexp]
  type abs = t(Index.absolute);
}

and Context: {
  [@deriving sexp]
  type binding =
    | VarBinding(Var.t, HTyp.rel)
    | TyVarBinding(TyVar.t, Kind.rel);

  [@deriving sexp]
  type t = list(binding);
};
