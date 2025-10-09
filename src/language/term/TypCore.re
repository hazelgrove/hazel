open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type term_core('type_provenance, 'typ_t, 'tpat_t) =
  | Unknown('type_provenance)
  | Atom(Atom.cls)
  | Var(string)
  | List('typ_t)
  | Arrow('typ_t, 'typ_t)
  | Sum(ConstructorMap.t('typ_t))
  | Prod(list('typ_t))
  | ExplicitNonlabel
  | Label(string)
  | TupLabel('typ_t, 'typ_t)
  | Parens('typ_t)
  | Rec('tpat_t, 'typ_t)
  | Forall('tpat_t, 'typ_t)
  | ProdProjection('typ_t, 'typ_t)
  | ProdExtension('typ_t, 'typ_t);
