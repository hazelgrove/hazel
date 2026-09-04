open Util;

/* Static messages: unified Message.t for inspector ok payloads. Info.derived_*
   enriches statics marks with ExpectationMismatch when ana/syn fail to meet, so
   marks on Info are authoritative for errors (no marks ⇒ no error). */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_ana =
  | Consistent({
      ana: Typ.t,
      syn: Typ.t,
      meet: Typ.t,
    })
  | InternallyInconsistent({
      ana: Typ.t,
      nomeet: list(Typ.t),
    });

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_common =
  | Syn(Typ.t)
  | Ana(ok_ana);

/* Non-error inspector payload for expressions / patterns. Principal syn type
   is Info.elab_syn_ty; Default is synthesis-only (no Ana payload). */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type exp =
  | Default
  | AnaDeferralConsistent(Typ.t)
  | Common(ok_common);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type pat =
  | Default
  | Common(ok_common);

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
