/**
  Optimization passes.
 */
[@deriving sexp]
type opts = unit;

/**
  The list of passes.
 */
let passes: list((opts, Mir_anf.block) => Mir_anf.block);

/**
  Perform optimizations on Anf.
 */
let optimize: (~opts: opts, Mir_anf.block) => Mir_anf.block;
