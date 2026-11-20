open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_variant =
  | Unique
  | Duplicate;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | TypeExpected
  /* Like `TypeExpected` but accepts higher-kinded types (kind
     `Arrow(...)`) without flagging them as `Type vs Arrow(...)`
     kind mismatches. Used when the type expression's surrounding
     context — typically a `type T = …` alias body — itself is
     happy to accept whatever kind the expression produces (the
     alias's kind is *defined* to be the body's kind). Avoids
     piling spurious kind-Type marks on every `TypFun` node of a
     curried alias body or every partial `TypParamAp`. */
  | AnyKindExpected
  | LabelExpected(status_variant, list(LabeledTuple.label))
  | LabelProjectionExpected(option(list(LabeledTuple.label)))
  | ProductExpected
  | ConstructorExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t);
