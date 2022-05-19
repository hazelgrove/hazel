/** Types with Holes.

   [HTyp]s are types that may have holes in them.
 */
module Context := KindSystem.Context;

/** ASTs for [HTyp]s. */
module Syntax = KindSystem.HTyp;

/* HTyp */

/** An (ordinary, opaque) [HTyp].

   In general, use this form of [HTyp] unless there is a good reason not to.

   Values of this type cannot be pattern matched against directly. Whenever
   possible, use the operations on opaque [HTyp]s provided below to ensure
   consistent handling of indices.
 */
[@deriving sexp]
type t;

/* HTyp Conversions */

/** Pruduces a brief description of an [HTyp]. */
let to_string: t => string;

/** Produces the underlying AST of an [HTyp]. */
let to_syntax: t => Syntax.abs;

/** Produces an [HTyp] with the given underlying AST. */
let of_syntax: Syntax.abs => t;

/* HTyp Constructors */

let hole: t;
let int: t;
let float: t;
let bool: t;
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

/** Structural equality of underlying ASTs. */
let equal: (t, t) => bool;

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

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
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

/**
   Normalized [HTyp]s contain no type variables of kind [S]. Since they contain
   no bound type variables, and therefore contain no bound indices, they are
   safe to pattern match against directly.
 */

/** A normalized [HTyp]. */
type normalized = Syntax.abs;

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

let grounded_Arrow: ground_cases;
let grounded_Sum: ground_cases;
let grounded_Prod: int => ground_cases;
let grounded_List: ground_cases;

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
