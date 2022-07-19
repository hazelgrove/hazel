/**
 * Linearize Hir into Anf.
 *
 * In this pass, all indet markers are initialized to true.
 */
let linearize: Hazelc_hir.Expr.expr => Anf.prog;
