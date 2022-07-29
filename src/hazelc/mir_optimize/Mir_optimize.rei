module IndetAnalysis = IndetAnalysis;

/**
  Optimization passes.
 */
[@deriving sexp]
type opts = {indet_analysis: IndetAnalysis.opts};

/**
  The list of passes.
 */
let passes: list((opts, Mir_anf.prog) => Mir_anf.prog);

/**
  Perform optimizations on Anf.
 */
let optimize: (~opts: opts, Mir_anf.prog) => Mir_anf.prog;
