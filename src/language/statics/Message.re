open Util;

/* Special inspector explanations. Ordinary type information and its
   consistency with an expectation are queried from Info, not stored here. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type exp =
  | Default
  | ArgumentDeferral;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type pat =
  | Default;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type underdetermined_typ =
  | ProdExtensionUnderdetermined(list(Typ.t))
  | ProdProjectionMissingLabel(LabeledTuple.label, list(LabeledTuple.label))
  | ProdProjectionBadArgs({
      product: option(Typ.t),
      label: option(Typ.t),
    });

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_typ =
  | Variant(Constructor.t, Typ.t)
  | TypeAlias(string, Typ.t)
  | WHNormalizedTo({
      unnormalized: Typ.t,
      whnormalized: Typ.t,
    })
  | Type(Typ.t)
  | EmptyLabel
  | TypeUnderdetermined(underdetermined_typ);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_tpat =
  | Empty
  | Var(string);

/* Single inspector payload for all sorts (non-error branch); errors use marks. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Exp(exp)
  | Pat(pat)
  | TypOk(ok_typ)
  | TPatOk(ok_tpat);
