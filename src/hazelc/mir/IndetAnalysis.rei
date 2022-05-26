/**
 * Perform static analysis to determine if each expression has any possibly
 * indeterminant sub-expression. These flags are computed and stored in each
 * expression.
 */

[@deriving sexp]
type analysis_level =
  | Local;

[@deriving sexp]
type opts = {analysis_level};

let analyze: (~opts: opts, Anf.prog) => Anf.prog;
