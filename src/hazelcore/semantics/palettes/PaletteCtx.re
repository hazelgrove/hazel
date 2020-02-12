[@deriving sexp]
type t = VarMap.t_(PaletteDefinition.t);
include VarMap /* TODO added to handle removing option return type from fix_holes functions */;

exception InvalidPaletteHoleName;
