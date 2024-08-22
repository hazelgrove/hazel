[@deriving sexp]
type t('a) = VarMap.t_(Typ.t('a));
include VarMap;
