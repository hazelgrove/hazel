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
let next: t => (Ident.t, t);

/**
 * Generate a new, named temporary variable name.
 */
let next_named: (Ident.t, t) => (Ident.t, t);
