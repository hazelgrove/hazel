/**
 * Optimization passes.
 */

[@deriving sexp]
type opts = {indet_analysis: IndetAnalysis.opts};

/**
 * The list of passes.
 */
let passes: list((opts, Anf.prog) => Anf.prog);

/**
 * Perform optimizations on Anf.
 */
let optimize: (~opts: opts, Anf.prog) => Anf.prog;
