/** A Simply Kinded Type System with Holes */

module rec HTyp: {
  [@deriving sexp]
  type t('idx) =
    | /** The unknown type. */
      Hole
    | /** Integers. */
      Int
    | /** Floats. */
      Float
    | /** Booleans. */
      Bool
    | /** Functions. */
      Arrow(t('idx), t('idx))
    | /** Binary sums. */
      Sum(t('idx), t('idx))
    | /** Finite products. */
      Prod(list(t('idx)))
    | /** Lists. */
      List(t('idx))
    | /** A bound type variable. */
      TyVar(Index.t('idx), TyVar.t)
    | /** A free type variable. */
      TyVarHole(
        TyVarErrStatus.HoleReason.t,
        MetaVar.t,
        TyVar.t,
      );

  /** Polymorphic structural equality of [HTyp]s.

     Determines if two [HTyp]s with the same type of indices are structurally
     equal.
   */
  let equal: (t('idx), t('idx)) => bool;

  /** An [HTyp] with relative indices. */
  [@deriving sexp]
  type rel = t(Index.relative);

  /** An [HTyp] with absolute indices. */
  [@deriving sexp]
  type abs = t(Index.absolute);

  /** Changes indices from absolute to relative. */
  let to_rel: (~offset: int=?, abs) => rel;

  /** Changes indices from relative to absolute. */
  let to_abs: (~offset: int=?, rel) => abs;
}

and Kind: {
  [@deriving sexp]
  type t('idx) =
    | /** The unknown kind. */
      Hole
    | /** The base kind. */
      HTyp
    | /** A singleton kind. */
      S(HTyp.t('idx));

  /** Polymorphic structural equality of [Kind]s.

     Determines if two [Kind]s with the same type of indices are structurally equal.
   */
  let equal: (t('idx), t('idx)) => bool;

  /** A [Kind] with relative indices. */
  [@deriving sexp]
  type rel = t(Index.relative);

  /** A [Kind] with absolute indices. */
  [@deriving sexp]
  type abs = t(Index.absolute);
}

and Context: {
  [@deriving sexp]
  type binding =
    | /** An expression variable binding. */
      VarBinding(Var.t, HTyp.rel)
    | /** A type variable binding. */
      TyVarBinding(TyVar.t, Kind.rel);

  [@deriving sexp]
  type t = list(binding);
};
