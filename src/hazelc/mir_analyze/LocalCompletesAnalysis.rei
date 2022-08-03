/**
  Perform static analysis to determine if each expression has any possibly
  indeterminant sub-expression.
 */

let analyze: Mir_anf.block => (Mir_anf.complete, Completes.t);
