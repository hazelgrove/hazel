/**
 * Perform static analysis to determine if each expression has any possibly
 * indeterminant sub-expression. These indet markers are computed and stored in
 * each expression.
 */

/**
 * Analysis level.
 *
 * TODO: Whole-program analysis for lambdas.
 */
[@deriving sexp]
type analysis_level =
  | Local;

/**
 * Options for analysis.
 */
[@deriving sexp]
type opts = {analysis_level};

/**
 * Perform static analysis and return annotated program.
 */
let analyze: (~opts: opts, Anf.prog) => Anf.prog;
