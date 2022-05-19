/** Typing Context.

   Combined typing contexts for both type variables and expression variables.
 */

/** A typing context. */
[@deriving sexp]
type t;

/** The initial typing context. */
let initial: t;

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

/** Binds a type variable with the given name to the given [Kind]. */
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
