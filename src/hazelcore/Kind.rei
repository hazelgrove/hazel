/** Simpe Kinds for Types with Holes.

   A [Kind] represents a class of [HTyp]s:
   - [Hole] is the {e unknown} [Kind]---the [Kind] of [HTyp] holes.
   - [HTyp] is the {e base} [Kind] of [HType]s without holes or type variables.
   - [S] is a {e singleton} [Kind] of [HType]s consistent with an underlying [HTyp].
 */
module HTyp := KindSystem.HTyp;
module Context := KindSystem.Context;

[@deriving sexp]
type s('idx) = KindSystem.Kind.t('idx) = | Hole | Type | S(HTyp.t('idx));

/** A [Kind]. */
[@deriving sexp]
type t = s(Index.absolute);

/** Produces the most general [HTyp] belonging to the given [Kind]. */
let to_htyp: t => HTyp.abs;

/** Constructs a singleton [Kind] with the given underlying type. */
let singleton: HTyp.abs => t;

/* Properties of Kind */

/** Subkind Consistency.

   One [Kind] is a consistent subkind of another when it satisfies one of the
   following properties:
   - At least one is of kind [Hole].
   - The first is at least as specific as the second.
   - Both are of kind [S] and have equivalent underlying types.
 */
let consistent_subkind: (Context.t, t, t) => bool;

/* Operations on Kind */

/** Type variable substitution. */
let subst_tyvars: (t, list((Index.Abs.t, HTyp.abs))) => t;
