open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_variant =
  | Unique
  | Duplicate;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | TypeExpected
  | LabelExpected(status_variant, list(LabeledTuple.label))
  | LabelProjectionExpected(option(list(LabeledTuple.label)))
  /* The label of a module path: a type member, or ([submodule]) a
     sub-module the path continues through. */
  | ModuleMemberExpected({
      members: list(Var.t),
      submodule: bool,
    })
  | ProductExpected
  | ConstructorExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t);
