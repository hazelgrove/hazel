/**
 * Perform static analysis to determine if each expression has any possibly
 * indeterminant sub-expression. These indet markers are computed and stored in
 * each expression.
 */

/**
 * Perform static analysis and return annotated program.
 */
let analyze: Mir_anf.prog => Mir_anf.prog;
