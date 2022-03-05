/* Errors related to EvalPostprocess.postprocess

   Postprocessing invalid cases:
   Evaluation boundary is abbreviated as "EB". "In closure" and "outside closure"
   correspond to "outside the EB" and "inside the EB," respectively.

   The following errors are used to indicate an invalid case DURING postprocessing.
   - ClosureInsideClosure: an evaluated expression outside the EB
   - BoundVarOutsideClosure: an un-looked-up (unevaluated) variable inside the EB
   - UnevalOutsideClosure: non-variable unevaluated expression inside the EB
   - InvalidClosureBody: closures currently only make sense storing the following
     expression types:
     - Hole expressions
     - Lambda abstractions
     - Let/case with a pattern match failure

   The following errors are used to indicate an invalid case AFTER postprocessing.
   After postprocessing, closures around lambda abstractions, let expressions, and
   case expressions should be removed, and all hole expressions should be wrapped
   in a closure.
   - PostprocessedNoneHoleInClosure
   - PostprocessedHoleOutsideClosure
   */
[@deriving sexp]
type t =
  | ClosureInsideClosure
  | BoundVarOutsideClosure(Var.t)
  | UnevalOutsideClosure
  | InvalidClosureBody
  | PostprocessedNonHoleInClosure
  | PostprocessedHoleOutsideClosure;

[@deriving sexp]
exception Exception(t);
