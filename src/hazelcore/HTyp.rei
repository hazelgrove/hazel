/** Types with Holes.

   [HTyp]s are types that may have holes in them.
 */

/** ASTs for [HTyp]s. */
module Syntax = KindSystem.HTyp;

/** An opaque [HTyp] value.

   Ensures all indices are absolutely positioned.
 */
[@deriving sexp]
type t;

/** Pruduces a brief description of an [HTyp]. */
let to_string: t => string;

/** Produces the underlying AST of an [HTyp]. */
let to_syntax: t => Syntax.abs;

/** Produces an [HTyp] with a given underlying AST. */
let of_syntax: Syntax.abs => t;

//** Structural equality of underlying ASTs. */
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
let consistent: (Contexts.t, t, t) => bool;

/** Context-sensitive type equivalence.

   Two types are equivalent when each pair of coinciding subterms satisfies one
   of the following properties:
   - They are structurally equal, up to type variables and type variable holes.
   - If one is a type variable hole, the other is a type variable hole with the
     same id.
 */
let equivalent: (Contexts.t, t, t) => bool;

/** A normalized [HTyp].

   Normalized [HTyp]s do not contain type variables of kind [S].
 */
/* type normalized = Syntax.abs; */
type normalized;

/** Normalizes an [HTyp].

   Replaces every type variable of kind [S] with its (recursively normalized)
   underlying type.
 */
let normalize: (Contexts.t, t) => normalized;

/** Promotes a normalized [HTyp] to an ordinary [HTyp].  */
let of_normalized: normalized => t;

/** Type consistency for normalized [HTyp]s. */
let normalized_consistent: (normalized, normalized) => bool;

/** Type equivalence for normalized [HTyp]s. */
let normalized_equivalent: (normalized, normalized) => bool;

/** A partial syntax for [HTyp]s that are not type variables of kind [S].

   Head-normalized [HTyp]s are "shallow" ASTs. They are like ASTs whose
   immediate children are ordinary [HTyp]s instead of AST nodes.

   This function is used by matched type constructors (see below) to pattern
   match on the underlying type of a type variable of kind [S].
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

/** Head-normalizes an [HTyp].

   If the [HTyp] is a type variable of kind [S], returns its (recursively
   head-normalized) underlying type. Otherwise, returns the [HTyp] unmodified.
 */
let head_normalize: (Contexts.t, t) => head_normalized;

/** Promotes a head-normalized [HTyp] to an ordinary [HTyp]. */
let of_head_normalized: head_normalized => t;

/** {1 [HTyp] Constructors} */

let tyvar: (Index.Abs.t, TyVar.t) => t;
let tyvarhole: (TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t) => t;
let hole: t;
let int: t;
let float: t;
let bool: t;
let arrow: (t, t) => t;
let sum: (t, t) => t;
let product: list(t) => t;
let list: t => t;

/** {1 General [HTyp] Properties} */

let is_hole: t => bool;

/** An [HTyp] is complete when it does not contain any holes. */
let complete: t => bool;

/** {1 [HTyp] Operator Precedence} */

let precedence_Prod: int;
let precedence_Arrow: int;
let precedence_Sum: int;
let precedence: t => int;

/** {1 Product Type Helpers} */

let get_prod_elements: head_normalized => list(t);
let get_prod_arity: head_normalized => int;

/** {1 Type Variable Helpers} */

let is_tyvar: t => bool;

let tyvar_index: t => option(Index.Abs.t);
let tyvar_name: t => option(TyVar.t);

/** Type variable substitution.

   Given a list of pairs of type variable indices and [HTyp]s, replaces each
   type variable with its paired type.
 */
let subst_tyvars: (t, list((Index.Abs.t, t))) => t;

/** {1 Matched Types} */

let matched_arrow: (Contexts.t, t) => option((t, t));
let matched_sum: (Contexts.t, t) => option((t, t));
let matched_list: (Contexts.t, t) => option(t);

/** {1 Joins} */

type join =
  | /** holes win */
    GLB
  | /** holes lose */
    LUB;

let join: (Contexts.t, join, t, t) => option(t);
let join_all: (Contexts.t, join, list(t)) => option(t);

/** {1 Ground Cases} */

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
