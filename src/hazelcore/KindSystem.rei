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

  /** Polymorphic structural equality of [HTyp]s.

     Determines if two [HTyp]s with the same type of indices are structurally
     equal.
   */
  let equal: (t('idx), t('idx)) => bool;

  /** Polymorphic equivalence of [HTyp]s.

     Determines if two [HTyp]s with the same type of indices are equivalent.
   */
  let equivalent: (Context.t, abs, abs) => bool;

  /** Polymorphic (on indices) type variable substitution.

     [subst_tyvar(ty, a, ty_a)] substitutes all occurrences of type variable [a]
     with [HTyp] [ty_a] in [HTyp] [ty].
   */
  let subst_tyvar: (t('idx), Index.t('idx), t('idx)) => t('idx);
  let subst_tyvars: (t('idx), list((Index.t('idx), t('idx)))) => t('idx);
}

and Kind: {
  [@deriving sexp]
  type t('idx) =
    | /** The unknown kind. */
      Hole
    | /** The base kind. */
      Type
    | /** A singleton kind. */
      S(HTyp.t('idx));

  /** A [Kind] with relative indices. */
  [@deriving sexp]
  type rel = t(Index.relative);

  /** A [Kind] with absolute indices. */
  [@deriving sexp]
  type abs = t(Index.absolute);

  /** Changes indices from absolute to relative. */
  let to_rel: (~offset: int=?, abs) => rel;

  /** Changes indices from relative to absolute. */
  let to_abs: (~offset: int=?, rel) => abs;

  /** Polymorphic conversion of [Kind]s to [HTyp]s. */
  let to_htyp: t('idx) => HTyp.t('idx);
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

  let nth_var_binding: (t, int) => option((int, Var.t, HTyp.rel));
  let nth_tyvar_binding: (t, int) => option((TyVar.t, Kind.rel));
  let first_tyvar_binding:
    (t, (TyVar.t, Kind.rel) => bool) => option((int, TyVar.t, Kind.rel));

  let tyvar_kind: (t, Index.Abs.t) => option(Kind.abs);
};
