open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_variant =
  | Unique
  | Duplicate;

/* What the label of a module path names: a type member, or a sub-module
   the path continues through (`P` in `M.P.T`). */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type module_member =
  | TypeMember
  | SubModule;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | TypeExpected
  | LabelExpected(status_variant, list(LabeledTuple.label))
  | LabelProjectionExpected(option(list(LabeledTuple.label)))
  /* The label of a module path, and the members of that sort it may name. */
  | ModuleMemberExpected(module_member, list(Var.t))
  | ProductExpected
  | ConstructorExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t);
