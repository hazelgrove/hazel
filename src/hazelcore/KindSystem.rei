/** Types with holes and type variables. */

module HTyp_syntax: {
  /** An HTyp. */
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

  /** Changes indices from absolute to relative. */
  let to_rel: (~offset: int=?, t(Index.absolute)) => t(Index.relative);

  /** Changes indices from relative to absolute. */
  let to_abs: (~offset: int=?, t(Index.relative)) => t(Index.absolute);

  /** Index-polymorphic type variable substitution.

     [subst_tyvar(ty, idx, new_ty)] substitutes [new_ty] for type variable instances
     with index [idx] in [ty].
   */
  let subst_tyvar: (t('idx), Index.t('idx), t('idx)) => t('idx);
  let subst_tyvars: (t('idx), list((Index.t('idx), t('idx)))) => t('idx);
};

module Kind_core: {
  /** A kind. */
  [@deriving sexp]
  type s('idx) =
    | /** An unknown kind. */
      Hole
    | /** A base kind. */
      Type
    | /** A singleton kind. */
      S(HTyp_syntax.t('idx));

  /** Changes indices from absolute to relative. */
  let to_rel: (~offset: int=?, s(Index.absolute)) => s(Index.relative);

  /** Changes indices from relative to absolute. */
  let to_abs: (~offset: int=?, s(Index.relative)) => s(Index.absolute);
};

/** A typing context.

   Combined typing contexts for both type variables and expression variables.
 */
module rec Context: {
  [@deriving sexp]
  type binding;

  /** A typing context. */
  [@deriving sexp]
  type t = list(binding);

  let to_list:
    t =>
    (
      list((Var.t, HTyp_syntax.t(Index.relative))),
      list((TyVar.t, Kind_core.s(Index.relative))),
    );

  /** Returns the number of binding in the given context */
  let length: t => int;

  /* Type Variables */

  /** Returns the absolute index, name, and [Kind] of each type variable bound by
   the given context. */
  let tyvars: t => list((Index.Abs.t, TyVar.t, Kind.t));

  /** Returns the name of the type variable bound at the given index. */
  let tyvar: (t, Index.Abs.t) => option(TyVar.t);

  /** Returns the index of the most recently bound type variable with the given
   name. */
  let tyvar_index: (t, TyVar.t) => option(Index.Abs.t);

  /** Returns the [Kind] of the type variable bound at the given index. */
  let tyvar_kind: (t, Index.Abs.t) => option(Kind.t);

  /** Binds the given type variable name to the given [Kind]. */
  let add_tyvar: (t, TyVar.t, Kind.t) => t;

  /** [diff_tyvars(ctx, ctx')] produces the absolute index and [HTyp] of any type
   variables bound in [ctx] but not in [ctx']. */
  let diff_tyvars: (t, t) => list((Index.Abs.t, HTyp.t));

  /* Expression Variables */

  /** Returns the absolute index, name, and [HTyp] of the expression variables
   bound by the given context. */
  let vars: t => list((Index.Abs.t, Var.t, HTyp.t));

  /** Returns the name of the expression variable bound at the given index. */
  let var: (t, Index.Abs.t) => option(Var.t);

  /** Returnx the index of the most recently bound expression variable with the
   given name. */
  let var_index: (t, Var.t) => option(Index.Abs.t);

  /** Returns the [HTyp] of the most recently bound expression variable with the
   given name. */
  let var_type: (t, Var.t) => option(HTyp.t);

  /** Binds an expression variable with the given name to the given [HTyp]. */
  let add_var: (t, Var.t, HTyp.t) => t;
}

/** Simpe kinds of types with holes.

   A [Kind] represents an equivalence class of [HTyp]s:
   - [Hole] - an unknown [Kind].
   - [Type] - the [Kind] of [HType]s without type variables.
   - [S(ty)] - a {e singleton} [Kind] of [HType]s consistent with an underlying [HTyp].
 */
and Kind: {
  [@deriving sexp]
  type t = Kind_core.s(Index.absolute);

  /** Produces the most general [HTyp] belonging to the given [Kind]. */
  let to_htyp: t => HTyp.t;

  /** Constructs a singleton [Kind] with the given underlying type. */
  let singleton: HTyp.t => t;

  /* Properties of Kind */

  /** Subkind consistency.

     One [Kind] is a consistent subkind of another when they satisfy one of the
     following properties:
     - At least one is [Hole].
     - The first is at least as specific as the second.
     - Both are singletons and their underlying types are equivalent.
   */
  let consistent_subkind: (Context.t, t, t) => bool;

  /** Kind equivalence.

     Two [Kind]s are equivalent when they satisfy one of the following
     properties:
     - Neither is a singleton.
     - Both are singletons and their underlying types are equivalent.
   */
  let equivalent: (Context.t, t, t) => bool;

  /* Operations on Kind */

  /** Type variable substitution. */
  let subst_tyvars: (t, list((Index.Abs.t, HTyp.t))) => t;
}

and HTyp: {
  /** An (ordinary, abstract, opaque) [HTyp].

     [HTyp]s of this form cannot be pattern matched against directly. Whenever
     possible, use abstract [HTyp]s to ensure consistent handling of indices.

     @see Hazel PHI 9
 */
  [@deriving sexp]
  type t;

  /* HTyp Conversions */

  /** Produces a brief description of an [HTyp]. */
  let to_string: t => string;

  /** Produces the underlying AST of an [HTyp]. */
  let to_syntax: t => HTyp_syntax.t(Index.absolute);

  /** Produces an [HTyp] with the given underlying AST. */
  let of_syntax: HTyp_syntax.t(Index.absolute) => t;

  /* Index Manipulation */

  /** Shifts all indices in the underlying AST by the given amount. */
  let shift_indices: (t, int) => t;

  /** Scope preserving cross-context index shifting.

     [rescope(new_ctx, old_ctx, tyvar(idx, t))] returns a
     [tyvar(idx', t)] satisfying
     [Index.equal(Context.tyvar_kind(old_ctx, idx), Context.tyvar_kind(old_ctx, idx))].
   */
  let rescope: (Context.t, Context.t, t) => t;

  /* HTyp Constructors */

  let hole: unit => t;
  let int: unit => t;
  let float: unit => t;
  let bool: unit => t;
  let arrow: (t, t) => t;
  let sum: (t, t) => t;
  let product: list(t) => t;
  let list: t => t;
  let tyvar: (Index.Abs.t, TyVar.t) => t;
  let tyvarhole: (TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t) => t;

  /* HTyp Value Predicates */

  let is_hole: t => bool;
  let is_tyvar: t => bool;

  /* Properties of HTyp */

  /** Context-sensitive type consistency.

     Two types are consistent when each pair of coinciding subterms satisfies one
     of the following properties:
     - They are structurally equal, up to holes and type variables.
     - At least one is a type variable hole or a type variable of kind [Hole] or
     [Type].
     - If one is a type variable of kind [S], the other is consistent with its
     underlying type.
     - If both are type variables of kind [S], their underlying types are
     consistent.
   */
  let consistent: (Context.t, t, t) => bool;

  /** Context-sensitive type equivalence.

     Two types are equivalent when each pair of coinciding subterms satisfies one
     of the following properties:
     - They are structurally equal, up to type variables and type variable holes.
     - If one is a type variable hole, the other is a type variable hole with the
     same id.
   */
  let equivalent: (Context.t, t, t) => bool;

  /** An [HTyp] is complete when it has no holes. */
  let complete: t => bool;

  /* HTyp Constructor Precedence */

  let precedence_Prod: unit => int;
  let precedence_Arrow: unit => int;
  let precedence_Sum: unit => int;
  let precedence: t => int;

  /* Matched Type Constructors */

  let matched_arrow: (Context.t, t) => option((t, t));
  let matched_sum: (Context.t, t) => option((t, t));
  let matched_list: (Context.t, t) => option(t);

  /* Type Variables */

  let tyvar_index: t => option(Index.Abs.t);
  let tyvar_name: t => option(TyVar.t);

  /** Type variable substitution.  */
  let subst_tyvar: (t, Index.Abs.t, t) => t;
  let subst_tyvars: (t, list((Index.Abs.t, t))) => t;

  /* Joins */

  type join =
    | /** holes win */
      GLB
    | /** holes lose */
      LUB;

  let join: (Context.t, join, t, t) => option(t);
  let join_all: (Context.t, join, list(t)) => option(t);

  /* HTyp Normalization */

  /** Normalized [HTyp]s contain no type variables of kind [S]. Since they
     contain no bound type variables, and therefore contain no bound indices,
     they are safe to pattern match against directly. */

  /** A normalized [HTyp]. */
  type normalized = HTyp_syntax.t(Index.absolute);

  /** Coerces a normalized [HTyp] to an ordinary [HTyp]. */
  let of_normalized: normalized => t;

  /** Normalizes an [HTyp].

     Replaces every type variable of kind [S] with its (recursively normalized)
     underlying type.
   */
  let normalize: (Context.t, t) => normalized;

  /* Properties of Normalized HTyp */

  /** Normalized [HTyp] consistency.

     WARNING: This function assumes all type variables are of kind [Hole] or
     [HTyp].
   */
  let normalized_consistent: (normalized, normalized) => bool;

  /** Normalized [HTyp] equivalence.

     WARNING: This function assumes all type variables are of kind [Hole] or
     [HTyp].
   */
  let normalized_equivalent: (normalized, normalized) => bool;

  /* Ground Cases */

  [@deriving sexp]
  type ground_cases =
    | Hole
    | Ground
    | NotGroundOrHole(normalized) /* the argument is the corresponding ground type */;

  let grounded_Arrow: unit => ground_cases;
  let grounded_Sum: unit => ground_cases;
  let grounded_Prod: int => ground_cases;
  let grounded_List: unit => ground_cases;

  let ground_cases_of: normalized => ground_cases;

  /* HTyp Head-Normalization */

  /**
     Head-normalized [HTyp]s are [HTyp]s that may contain type variables of any
     kind, but that are not themselves type variables of kind [S]. Since any bound
     indices would have to be contained by subterms, the outermost AST node of a
     head-normalized [HTyp] is safe to pattern match against directly.
   */

  /** A "shallow" AST for [HTyp]s that are not type variables of kind [S].

     Head-normalized [HTyp]s are ASTs whose immediate children are ordinary
     [HTyp]s instead of AST nodes.

     This function is used by matched type constructors (see above) to convert
     a type variable of kind [S] to its underlying .
   */
  [@deriving sexp]
  type head_normalized =
    | Hole
    | Int
    | Float
    | Bool
    | Arrow(t, t)
    | Sum(t, t)
    | Prod(list(t))
    | List(t)
    | TyVar(Index.Abs.t, TyVar.t)
    | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t);

  /** Converts a head-normalized [HTyp] to an ordinary [HTyp]. */
  let of_head_normalized: head_normalized => t;

  /** Head-normalizes an [HTyp].

     If the [HTyp] is a type variable of kind [S], returns its (recursively
     head-normalized) underlying type. Otherwise, returns the [HTyp] unmodified.
   */
  let head_normalize: (Context.t, t) => head_normalized;

  /* Product Types */

  let get_prod_elements: head_normalized => list(t);
  let get_prod_arity: head_normalized => int;
};
