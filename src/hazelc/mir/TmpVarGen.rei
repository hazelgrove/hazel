/**
 * Temporary variable name generator.
 */
[@deriving sexp]
type t;

/**
 * Initialize a generator.
 */
let init: t;

/**
 * Generate a new temporary variable name.
 */
let next: t => (Var.t, t);

/**
 * Generate a new, named temporary variable name.
 */
let next_named: (Var.t, t) => (Var.t, t);
