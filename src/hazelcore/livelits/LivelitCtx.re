open Sexplib.Std;

[@deriving sexp]
type t('closed_arg) =
  VarMap.t_((LivelitDefinition.t, list((Var.t, 'closed_arg))));
include VarMap;

/* TODO added to handle removing option return type from fix_holes functions */
exception InvalidLivelitHoleName;
