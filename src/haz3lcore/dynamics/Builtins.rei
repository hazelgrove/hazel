[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Builtin.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_((DHExp.t, Builtin.builtin_evaluate));

/**
  [ctx builtins] is the static type context of the builtins.
 */
let ctx: ElaboratorMonad.t(t) => Ctx.t;

/**
  [elabs builtins] is the map of the semi-dynamic forms of the builtins.
 */
let elabs: ElaboratorMonad.t(t) => ElaboratorMonad.t(forms);

/**
  [forms builtins] is the map of the dynamic forms of the builtins.
 */
let forms: ElaboratorMonad.t(t) => forms;

/**
  [using name impl builtins] extends the map [builtins] with the builtin given
  by [impl] with name [name].
 */
let using:
  (Var.t, Var.t => ElaboratorMonad.t(Builtin.t), ElaboratorMonad.t(t)) =>
  ElaboratorMonad.t(t);

/**
  Module of some builtin functions.
 */
module Pervasives: {
  let pi: Var.t => ElaboratorMonad.t(Builtin.t);

  let int_of_float: Var.t => ElaboratorMonad.t(Builtin.t);
  let float_of_int: Var.t => ElaboratorMonad.t(Builtin.t);
  let modulo: Var.t => ElaboratorMonad.t(Builtin.t);

  let builtins: ElaboratorMonad.t(t);
};
