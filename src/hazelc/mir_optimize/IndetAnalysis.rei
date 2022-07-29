/**
 * Analysis level.
 */
[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis
  | GlobalAnalysis;

/**
 * Options for analysis.
 */
[@deriving sexp]
type opts = {level};

let analyze: (~opts: opts, Mir_anf.prog) => Mir_anf.prog;
