[@deriving (sexp, show)]
type t = VarMap.t_(PaletteDefinition.t);
include (module type of VarMap);

/* TODO added to handle removing option return type from fix_holes functions */
exception InvalidPaletteHoleName;
