/* Type-directed suggestion sets, keyed by what the cursor can accept. The
   form tables and filters behind them are private. */

let suggest_operator: Language.Info.t => list(TyDiSuggestion.t);

let suggest_operand: Language.Info.t => list(TyDiSuggestion.t);

let suggest_leading: Language.Info.t => list(TyDiSuggestion.t);
