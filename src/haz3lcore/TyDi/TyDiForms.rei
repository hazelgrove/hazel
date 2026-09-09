/* Type-directed suggestions for TyDi, filtered to the forms that fit the type
   the cursor's position expects. One function per syntactic slot, since an
   operator, an operand and a leading form admit different candidates at the
   same `Info.t`. */

let suggest_operator: Language.Info.t => list(TyDiSuggestion.t);

let suggest_operand: Language.Info.t => list(TyDiSuggestion.t);

let suggest_leading: Language.Info.t => list(TyDiSuggestion.t);
