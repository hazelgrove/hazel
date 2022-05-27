/**
 * Perform static analysis to determine if each expression has any possibly
 * indeterminant sub-expression. These indet markers are computed and stored in
 * each expression.
 */

/**
 * Analysis level.
 *
 * TODO: Whole-program analysis (e.g. for lambdas).
 */
[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis;

/**
 * Options for analysis.
 */
[@deriving sexp]
type opts = {level};

/**
 * Perform static analysis and return annotated program.
 */
let analyze: (~opts: opts, Anf.prog) => Anf.prog;
