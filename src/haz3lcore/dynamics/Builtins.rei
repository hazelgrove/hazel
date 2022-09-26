[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_((Typ.t, DHExp.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_((DHExp.t, Builtin.builtin_evaluate));

/**
  [to_ctx builtins] is the static type context of the builtins.
 */
let to_ctx: t => Ctx.t;

/**
  [to_forms builtins] is the map of the dynamic forms of the builtins.
 */
let to_forms: t => forms;
