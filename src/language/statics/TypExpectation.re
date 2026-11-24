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

let module_member_to_string =
  fun
  | TypeMember => "type member"
  | SubModule => "sub-module";

/* The members of one sort a module path's label may name, and whether a
   hole item among the signature's items could declare another. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type module_members = {
  listed: list(Var.t),
  hole: bool,
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | TypeExpected
  | LabelExpected(status_variant, list(LabeledTuple.label))
  | LabelProjectionExpected(option(list(LabeledTuple.label)))
  /* The label of a module path, and the members it may name. */
  | ModuleMemberExpected(module_member, module_members)
  | ProductExpected
  | ConstructorExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t);
