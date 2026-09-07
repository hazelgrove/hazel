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
      /* Abstract members reached through a signature alias rather than a
         module: no module is named, so nothing names them. */
      unnameable: list(Var.t),
    })
  | ProductExpected
  /* A component of an arrow domain: the one place an implicit binder type
     may appear. Otherwise as TypeExpected. */
  | ArrowDomainExpected
  | ConstructorExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t);
