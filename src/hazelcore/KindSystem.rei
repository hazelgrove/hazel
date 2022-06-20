/** A self-correcting reference into a typing context. */
module ContextRef: {
  [@deriving sexp]
  type s('idx) = {
    /** An index into some context. */
    index: Index.t('idx),
    /** The length of the context used to create the reference. */
    stamp: int,
  };

  [@deriving sexp]
  type t = s(Index.absolute);

  let equal: (t, t) => bool;
};

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
      TyVar(ContextRef.s('idx), TyVar.t)
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

     [subst_tyvar(ty, idx, new_ty)] substitutes [new_ty] for type variable at
     index [idx] in [ty].
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

  /** Converts a reference between two different versions of a context.

     WARNING: result is only valid if the given context is the same context
     used to construct the reference, or an extension of it.
   */
  let rescope: (t, ContextRef.t) => ContextRef.t;

  /* Type Variables */

  /** Returns a reference, name, and [Kind] for each type variable bound in the
     given context. */
  let tyvars: t => list((ContextRef.t, TyVar.t, Kind.t));

  /** Returns the name of the referenced type variable. */
  let tyvar: (t, ContextRef.t) => option(TyVar.t);

  /** Returns a reference to the type variable most recently bound to the given
     name. */
  let tyvar_ref: (t, TyVar.t) => option(ContextRef.t);

  /** Returns the [Kind] of the referenced type variable. */
  let tyvar_kind: (t, ContextRef.t) => option(Kind.t);

  /** Binds the given type variable name to the given [Kind]. */
  let add_tyvar: (t, TyVar.t, Kind.t) => t;

  /** [reduce_tyvars(new_ctx, old_ctx, ty)] replaces any type variables bound by
     [new_ctx] but not by [old_ctx] in [ty] with equivalent types that have no
     new type variables.

     WARNING: result only valid if the contexts are the same or one is an
     extension of the other.
   */
  let reduce_tyvars: (t, t, HTyp.t) => HTyp.t;

  /* Expression Variables */

  /** Returns a reference, name, and [HTyp] for every expression variable bound
   in the given context. */
  let vars: t => list((ContextRef.t, Var.t, HTyp.t));

  /** Returns the name of the referenced expression variable. */
  let var: (t, ContextRef.t) => option(Var.t);

  /** Returnx a reference to the expression variable most recently bound to the
     given name. */
  let var_ref: (t, Var.t) => option(ContextRef.t);

  /** Returns the [HTyp] of the expression variable most recently bound to the
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

  /** Scope preserving cross-context index shifting. */
  let rescope: (Context.t, t) => t;

  /* Base HTyp Constructors */

  let hole: unit => t;
  let int: unit => t;
  let float: unit => t;
  let bool: unit => t;
  let arrow: (t, t) => t;
  let sum: (t, t) => t;
  let product: list(t) => t;
  let list: t => t;

  /* HTyp Value Predicates */

  let is_hole: t => bool;
  let is_int: t => bool;
  let is_float: t => bool;
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

  let tyvar: (Context.t, Index.Abs.t, TyVar.t) => t;
  let tyvarhole: (TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t) => t;

  let tyvar_ref: t => option(ContextRef.t);
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
    | TyVar(ContextRef.t, TyVar.t)
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
