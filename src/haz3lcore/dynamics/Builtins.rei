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
  [using name impl builtins] extends the map [builtins] with the builtin given
  by [impl] with name [name].
 */
let using: (Var.t, Var.t => Builtin.t, t) => t;

/**
  Module of some builtin functions.
 */
module Pervasives: {
  let pi: Var.t => Builtin.t;
  let infinity: Var.t => Builtin.t;
  let nan: Var.t => Builtin.t;
  let neg_infinity: Var.t => Builtin.t;

  let is_infinite: Var.t => Builtin.t;
  let is_nan: Var.t => Builtin.t;
  let int_of_float: Var.t => Builtin.t;
  let float_of_int: Var.t => Builtin.t;
  let modulo: Var.t => Builtin.t;

  let builtins: t;
};
