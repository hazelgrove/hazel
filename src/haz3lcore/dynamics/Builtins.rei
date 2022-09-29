[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Builtin.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_((DHExp.t, Builtin.builtin_evaluate));

/**
  [ctx builtins] is the static type context of the builtins.
 */
let ctx: t => Ctx.t;

/**
  [forms builtins] is the map of the dynamic forms of the builtins.
 */
let forms: t => forms;

/**
  Module of some builtin functions.
 */
module Pervasives: {
  let pi: string => Builtin.t;

  let int_of_float: string => Builtin.t;
  let float_of_int: string => Builtin.t;
  let modulo: string => Builtin.t;
};
