[@deriving sexp]
type t('additional_data) = VarMap.t_((HTyp.t, 'additional_data));
include VarMap;
