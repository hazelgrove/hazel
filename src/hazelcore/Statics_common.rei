[@deriving sexp]
type edit_state = (ZExp.t, HTyp.t, MetaVarGen.t);

type type_mode =
  | Syn
  | Ana(HTyp.t);

let tuple_zip:
  (
    ~get_tuple_elements: Skel.t('op) => list(Skel.t('op)),
    Skel.t('op),
    HTyp.t
  ) =>
  option(list((Skel.t('op), HTyp.t)));
