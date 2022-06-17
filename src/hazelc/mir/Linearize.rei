/**
 * Linearize Hir into Anf.
 *
 * In this pass, all indet markers are initialized to true.
 */
let linearize: Hir.expr => Anf.prog;
