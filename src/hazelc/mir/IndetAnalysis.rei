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

let analyze: (~opts: opts, Anf.prog) => Anf.prog;
